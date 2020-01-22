// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats._
import cats.data._
import cats.effect.{Async, Timer}
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import mouse.boolean._
import monocle.macros.Lenses
import squants.Length
import seqexec.model.enum.Instrument
import seqexec.model.enum.ComaOption
import seqexec.model.enum.MountGuideOption
import seqexec.model.enum.M1Source
import seqexec.model.enum.TipTiltSource
import seqexec.model.M2GuideConfig
import seqexec.model.M1GuideConfig
import seqexec.model.TelescopeGuideConfig
import seqexec.server.altair.Altair
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.tcs.TcsController._
import seqexec.server.EpicsCodex.encode
import seqexec.server.SeqexecFailure
import seqexec.server.tcs.Gaos._
import seqexec.server.tcs.TcsNorthController.TcsNorthAoConfig
import shapeless.tag.@@
import squants.space.Arcseconds
import squants.time.TimeConversions._

import java.time.Duration

trait TcsNorthControllerEpicsAo[F[_]] {
  def applyAoConfig(subsystems: NonEmptySet[Subsystem],
                    gaos: Altair[F],
                    tcs: TcsNorthAoConfig): F[Unit]
}

object TcsNorthControllerEpicsAo {
  private def aoOffsetThreshold(instrument: Instrument): Option[Length] = instrument match {
    case Instrument.Nifs  => (Arcseconds(0.01)/FOCAL_PLANE_SCALE).some
    case Instrument.Niri  => (Arcseconds(3.0)/FOCAL_PLANE_SCALE).some
    case Instrument.Gnirs => (Arcseconds(3.0)/FOCAL_PLANE_SCALE).some
    case _                => none
  }

    private def mustPauseWhileOffsetting(current: EpicsTcsAoConfig, demand: TcsNorthAoConfig): Boolean = {
      val distanceSquared = demand.tc.offsetA.map(_.toFocalPlaneOffset(current.base.iaa))
        .map { o => (o.x - current.base.offset.x, o.y - current.base.offset.y) }
        .map(d => d._1 * d._1 + d._2 * d._2)

      val aoThreshold = aoOffsetThreshold(demand.inst.instrument)
          .filter(_ => Tcs.calcGuiderInUse(demand.gc, TipTiltSource.GAOS, M1Source.GAOS) && demand.gds.aoguide.isActive)

      val thresholds = List(
        (Tcs.calcGuiderInUse(demand.gc, TipTiltSource.PWFS1, M1Source.PWFS1) && demand.gds.pwfs1.isActive)
          .option(pwfs1OffsetThreshold),
        aoThreshold,
        demand.inst.oiOffsetGuideThreshold
          .filter(_ => Tcs.calcGuiderInUse(demand.gc, TipTiltSource.OIWFS, M1Source.OIWFS) && demand.gds.oiwfs.isActive)
      )
      // Does the offset movement surpass any of the existing thresholds ?
      distanceSquared.exists(dd => thresholds.exists(_.exists(t => t*t < dd)))
    }

  private final class TcsNorthControllerEpicsAoImpl[F[_]: Async: Timer](epicsSys: TcsEpics[F])(implicit L: Logger[F]) extends TcsNorthControllerEpicsAo[F] with TcsControllerEncoders {
    private val tcsConfigRetriever = TcsConfigRetriever[F](epicsSys)
    private val commonController = TcsControllerEpicsCommon[F](epicsSys)

    private def setAltairProbe(subsystems: NonEmptySet[Subsystem], c: ProbeTrackingConfig, d: ProbeTrackingConfig)
    : Option[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] =
      if(subsystems.contains(Subsystem.Gaos)) {
        val actions = List(
          (c.getNodChop =!= d.getNodChop)
            .option(commonController.setNodChopProbeTrackingConfig(epicsSys.pwfs2ProbeGuideCmd)(d.getNodChop)),
          (c.follow =!= d.follow).option(epicsSys.aoProbeFollowCmd.setFollowState(encode(d.follow)))
        ).collect{ case Some(x) => x }

        actions.nonEmpty.option{ x => actions.sequence *>
          EpicsTcsAoConfig.aowfs.set(d)(x).pure[F]
        }
      }
      else none

    override def applyAoConfig(subsystems: NonEmptySet[Subsystem],
                             gaos: Altair[F],
                             tcs: TcsNorthAoConfig): F[Unit] = {
      def configParams(current: EpicsTcsAoConfig): List[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] = List(
        commonController.setPwfs1Probe(EpicsTcsAoConfig.base)(subsystems, current.base.pwfs1.tracking, tcs.gds.pwfs1.tracking),
        setAltairProbe(subsystems, current.aowfs, tcs.gds.aoguide.tracking),
        commonController.setOiwfsProbe(EpicsTcsAoConfig.base)(subsystems, current.base.oiwfs.tracking, tcs.gds.oiwfs.tracking),
        tcs.tc.offsetA.flatMap(o => TcsControllerEpicsCommon.applyParam(subsystems.contains(Subsystem.Mount), current.base.offset,
          o.toFocalPlaneOffset(current.base.iaa), commonController.setTelescopeOffset, EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.offset
        )),
        tcs.tc.wavelA.flatMap(TcsControllerEpicsCommon.applyParam(subsystems.contains(Subsystem.Mount), current.base.wavelA, _, commonController.setWavelength,
          EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.wavelA
        )),
        commonController.setScienceFold(EpicsTcsAoConfig.base)(subsystems, current, tcs.agc.sfPos),
        commonController.setHrPickup(EpicsTcsAoConfig.base)(subsystems, current, tcs.agc)
      ).flattenOption

      def sysConfig(current: EpicsTcsAoConfig): F[EpicsTcsAoConfig] = {
        val params = configParams(current)
        val stabilizationTime = tcs.tc.offsetA
          .map(TcsSettleTimeCalculator.calc(current.base.instrumentOffset, _, subsystems, tcs.inst.instrument))
          .getOrElse(0.seconds)


        if(params.nonEmpty)
          for {
            s <- params.foldLeft(current.pure[F]){ case (c, p) => c.flatMap(p) }
            _ <- epicsSys.post(TcsControllerEpicsCommon.ConfigTimeout)
            _ <- L.debug("TCS configuration command post")
            _ <- if(subsystems.contains(Subsystem.Mount))
              epicsSys.waitInPosition(Duration.ofMillis(stabilizationTime.toMillis), tcsTimeout) *> L.debug("TCS inposition")
            else if(Set(Subsystem.PWFS1, Subsystem.PWFS2, Subsystem.AGUnit).exists(subsystems.contains))
              epicsSys.waitAGInPosition(agTimeout) *> L.debug("AG inposition")
            else Applicative[F].unit
          } yield s
        else
          L.debug("Skipping TCS configuration") *> current.pure[F]
      }

      for {
        s0 <- tcsConfigRetriever.retrieveConfigurationNorth(gaos.isFollowing)
        _  <- SeqexecFailure.Execution("Found useAo not set for AO step.").raiseError[F, Unit].whenA(!s0.base.useAo)
        pr <- pauseResumeGaos(gaos, s0, tcs)
        _  <- pr.pause.getOrElse(Applicative[F].unit)
        s1 <- guideOff(subsystems, s0, tcs, pr.pause.isEmpty)
        s2 <- sysConfig(s1)
        _  <- guideOn(subsystems, s2, tcs, pr.resume.isDefined)
        _  <- pr.resume.getOrElse(Applicative[F].unit) //resume Gaos
      } yield ()
    }

    def guideParams(subsystems: NonEmptySet[Subsystem], current: EpicsTcsAoConfig, demand: TcsNorthAoConfig)
    : List[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] = List(
      commonController.setMountGuide(EpicsTcsAoConfig.base)(subsystems, current.base.telescopeGuideConfig.mountGuide, demand.gc.mountGuide),
      commonController.setM1Guide(EpicsTcsAoConfig.base)(subsystems, current.base.telescopeGuideConfig.m1Guide, demand.gc.m1Guide),
      commonController.setM2Guide(EpicsTcsAoConfig.base)(subsystems, current.base.telescopeGuideConfig.m2Guide, demand.gc.m2Guide),
      commonController.setPwfs1(EpicsTcsAoConfig.base)(subsystems, current.base.pwfs1.detector, demand.gds.pwfs1.detector),
      commonController.setOiwfs(EpicsTcsAoConfig.base)(subsystems, current.base.oiwfs.detector, demand.gds.oiwfs.detector)
    ).flattenOption

    def calcGuideOff(current: EpicsTcsAoConfig, demand: TcsNorthAoConfig, gaosEnabled: Boolean): TcsNorthAoConfig = {
      val mustOff = mustPauseWhileOffsetting(current, demand)
      // Only turn things off here. Things that must be turned on will be turned on in GuideOn.
      def calc(c: GuiderSensorOption, d: GuiderSensorOption) = (mustOff || d === GuiderSensorOff).fold(GuiderSensorOff, c)

      (AoTcsConfig.gds[GuiderConfig@@AoGuide, AltairConfig].modify(
        (AoGuidersConfig.pwfs1[GuiderConfig@@AoGuide] ^<-> tagIso ^|-> GuiderConfig.detector)
          .set(calc(current.base.pwfs1.detector, demand.gds.pwfs1.detector)) >>>
          (AoGuidersConfig.oiwfs[GuiderConfig@@AoGuide] ^<-> tagIso ^|-> GuiderConfig.detector)
            .set(calc(current.base.oiwfs.detector, demand.gds.oiwfs.detector))
      ) >>> AoTcsConfig.gc[GuiderConfig@@AoGuide, AltairConfig].modify(
        TelescopeGuideConfig.mountGuide.set(
          (mustOff || demand.gc.mountGuide === MountGuideOption.MountGuideOff).fold(MountGuideOption.MountGuideOff, current.base.telescopeGuideConfig.mountGuide)
        ) >>>
          TelescopeGuideConfig.m1Guide.set(
            (mustOff || demand.gc.m1Guide === M1GuideConfig.M1GuideOff).fold(M1GuideConfig.M1GuideOff, current.base.telescopeGuideConfig.m1Guide)
          ) >>>
          TelescopeGuideConfig.m2Guide.set(
            (mustOff || demand.gc.m2Guide === M2GuideConfig.M2GuideOff).fold(M2GuideConfig.M2GuideOff, current.base.telescopeGuideConfig.m2Guide)
          )
      ) >>> normalizeM1Guiding(gaosEnabled) >>> normalizeM2Guiding(gaosEnabled) >>> normalizeMountGuiding)(demand)
    }

    def calcAoPauseConditions(current: EpicsTcsAoConfig, demand: TcsNorthAoConfig): PauseConditionSet =
      PauseConditionSet.fromList(
        List(
          demand.tc.offsetA.flatMap(v => (v =!= current.base.instrumentOffset)
            .option(PauseCondition.OffsetMove(v.toFocalPlaneOffset(current.base.iaa)))),
          (current.base.oiwfs.detector === GuiderSensorOn && demand.gds.oiwfs.detector === GuiderSensorOff).option(PauseCondition.OiOff),
          (current.base.pwfs1.detector === GuiderSensorOn && demand.gds.pwfs1.detector === GuiderSensorOff).option(PauseCondition.P1Off),
          (demand.gds.aoguide.detector === GuiderSensorOff).option(PauseCondition.GaosGuideOff)
        ).flattenOption
      )

    def calcAoResumeConditions(current: EpicsTcsAoConfig, demand: TcsNorthAoConfig): ResumeConditionSet =
      ResumeConditionSet.fromList(
        List(
          demand.tc.offsetA.map(v => ResumeCondition.OffsetReached(v.toFocalPlaneOffset(current.base.iaa))),
          (demand.gds.oiwfs.detector === GuiderSensorOn).option(ResumeCondition.OiOn),
          (demand.gds.pwfs1.detector === GuiderSensorOn).option(ResumeCondition.P1On),
          (demand.gds.aoguide.detector === GuiderSensorOn).option(ResumeCondition.GaosGuideOn)
        ).flattenOption
      )

    def pauseResumeGaos(gaos: Altair[F], current: EpicsTcsAoConfig, demand: TcsNorthAoConfig)
    : F[PauseResume[F]] =
      gaos.pauseResume(demand.gaos, calcAoPauseConditions(current, demand), calcAoResumeConditions(current, demand))

    def updateEpicsGuideConfig(epicsCfg: EpicsTcsAoConfig, demand: TcsNorthAoConfig): EpicsTcsAoConfig = (
      (EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.telescopeGuideConfig).set(demand.gc) >>>
        (EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.pwfs1 ^|-> GuiderConfig.detector).set(demand.gds.pwfs1.detector) >>>
        (EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.oiwfs ^|-> GuiderConfig.detector).set(demand.gds.oiwfs.detector)
      )(epicsCfg)

    def guideOff(subsystems: NonEmptySet[Subsystem], current: EpicsTcsAoConfig, demand: TcsNorthAoConfig, pauseGaos: Boolean)
    : F[EpicsTcsAoConfig] = {
      val params = guideParams(subsystems, current, calcGuideOff(current, demand, pauseGaos) )

      if(params.nonEmpty)
        for {
          s <- params.foldLeft(current.pure[F]){ case (c, p) => c.flatMap(p)}
          _ <- epicsSys.post(TcsControllerEpicsCommon.DefaultTimeout)
          _ <- L.debug("Turning guide off")
        } yield s
      else
        L.debug("Skipping guide off") *> current.pure[F]
    }

    def guideOn(subsystems: NonEmptySet[Subsystem], current: EpicsTcsAoConfig, demand: TcsNorthAoConfig,
                gaosEnabled: Boolean): F[EpicsTcsAoConfig] = {
      // If the demand turned off any WFS, normalize will turn off the corresponding processing
      val normalizedGuiding = (normalizeM1Guiding(gaosEnabled) >>> normalizeM2Guiding(gaosEnabled) >>>
        normalizeMountGuiding)(demand)

      val params = guideParams(subsystems, current, normalizedGuiding)

      if(params.nonEmpty)
        for {
          s <- params.foldLeft(current.pure[F]){ case (c, p) => c.flatMap(p)}
          _ <- epicsSys.post(TcsControllerEpicsCommon.DefaultTimeout)
          _ <- L.debug("Turning guide on")
        } yield s
      else
        L.debug("Skipping guide on") *> current.pure[F]
    }

    // Disable M1 guiding if source is off
    def normalizeM1Guiding(gaosEnabled: Boolean): Endo[TcsNorthAoConfig] = cfg =>
      (AoTcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).modify{
        case g @ M1GuideConfig.M1GuideOn(src) => src match {
          case M1Source.PWFS1 => if(cfg.gds.pwfs1.isActive) g else M1GuideConfig.M1GuideOff
          case M1Source.OIWFS => if(cfg.gds.oiwfs.isActive) g else M1GuideConfig.M1GuideOff
          case M1Source.GAOS  => if(cfg.gds.aoguide.detector === GuiderSensorOn && gaosEnabled) g
                                 else M1GuideConfig.M1GuideOff
          case _              => g
        }
        case x                => x
      }(cfg)

    // Disable M2 sources if they are off, disable M2 guiding if all are off
    def normalizeM2Guiding(gaosEnabled: Boolean): Endo[TcsNorthAoConfig] = cfg =>
      (AoTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).modify{
        case M2GuideConfig.M2GuideOn(coma, srcs) =>
          val ss = srcs.filter{
            case TipTiltSource.PWFS1 => cfg.gds.pwfs1.isActive
            case TipTiltSource.OIWFS => cfg.gds.oiwfs.isActive
            case TipTiltSource.GAOS  => cfg.gds.aoguide.detector === GuiderSensorOn && gaosEnabled
            case _                   => true
          }
          if(ss.isEmpty) M2GuideConfig.M2GuideOff
          else M2GuideConfig.M2GuideOn((cfg.gc.m1Guide =!= M1GuideConfig.M1GuideOff).fold(coma, ComaOption.ComaOff), ss)
        case x                     => x
      }(cfg)

    // Disable Mount guiding if M2 guiding is disabled
    val normalizeMountGuiding: Endo[TcsNorthAoConfig] = cfg =>
      (AoTcsConfig.gc ^|-> TelescopeGuideConfig.mountGuide).modify{ m => (m, cfg.gc.m2Guide) match {
        case (MountGuideOption.MountGuideOn, M2GuideConfig.M2GuideOn(_, _)) => MountGuideOption.MountGuideOn
        case _                                                              => MountGuideOption.MountGuideOff
      } }(cfg)

  }

  def apply[F[_]: Async: Logger: Timer](epicsSys: TcsEpics[F]): TcsNorthControllerEpicsAo[F] =
    new TcsNorthControllerEpicsAoImpl(epicsSys)

  @Lenses
  final case class EpicsTcsAoConfig(
    base: BaseEpicsTcsConfig,
    aowfs: ProbeTrackingConfig
  )

}
