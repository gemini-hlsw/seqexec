// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats._
import cats.implicits._
import cats.data.NonEmptySet
import cats.effect.{Async, Sync, Timer}
import io.chrisdavenport.log4cats.Logger
import monocle.Lens
import mouse.boolean._
import monocle.macros.Lenses
import seqexec.model.M1GuideConfig.M1GuideOn
import seqexec.model.{M1GuideConfig, M2GuideConfig, TelescopeGuideConfig}
import seqexec.model.enum.{ComaOption, M1Source, MountGuideOption, TipTiltSource}
import seqexec.server.EpicsCodex.encode
import seqexec.server.SeqexecFailure
import seqexec.server.gems.Gems
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.tcs.Gaos.{PauseCondition, PauseConditionSet, ResumeCondition, ResumeConditionSet}
import seqexec.server.tcs.GemsSource._
import seqexec.server.tcs.TcsController.{AoGuidersConfig, AoTcsConfig, GuiderConfig, GuiderSensorOff, GuiderSensorOption, ProbeTrackingConfig, Subsystem, wavelengthEq}
import seqexec.server.tcs.TcsEpics.{ProbeFollowCmd, VirtualGemsTelescope}
import seqexec.server.tcs.TcsSouthController.{GemsGuiders, TcsSouthAoConfig}
import squants.time.TimeConversions._

import java.time.Duration

/**
 * Controller of Gemini's South AO system over epics
 */
sealed trait TcsSouthControllerEpicsAo[F[_]] {
  def applyAoConfig(
    subsystems: NonEmptySet[Subsystem],
    gaos: Gems[F],
    baseAoConfig: GemsConfig,
    tcs: TcsSouthAoConfig
  ): F[Unit]
}

object TcsSouthControllerEpicsAo {
  @Lenses
  final case class EpicsTcsAoConfig(
    base: BaseEpicsTcsConfig,
    mapping: Map[GemsSource, VirtualGemsTelescope],
    cwfs1: GuiderConfig,
    cwfs2: GuiderConfig,
    cwfs3: GuiderConfig,
    odgw1: GuiderConfig,
    odgw2: GuiderConfig,
    odgw3: GuiderConfig,
    odgw4: GuiderConfig
  )

  private final class TcsSouthControllerEpicsAoImpl[F[_]: Async: Timer](epicsSys: TcsEpics[F])(
    implicit L: Logger[F]) extends TcsSouthControllerEpicsAo[F] with TcsControllerEncoders {
    private val tcsConfigRetriever = TcsConfigRetriever[F](epicsSys)
    private val commonController = TcsControllerEpicsCommon[F](epicsSys)

    def setNgsGuide(followCmd: ProbeFollowCmd[F], l: Lens[EpicsTcsAoConfig, GuiderConfig])(
      g: VirtualGemsTelescope,
      subsystems: NonEmptySet[Subsystem],
      current: ProbeTrackingConfig,
      demand: ProbeTrackingConfig
    ): Option[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] =
      if (subsystems.contains(Subsystem.Gaos)) {
        val actions = List(
          (current.getNodChop =!= demand.getNodChop)
            .option(
              commonController
                .setNodChopProbeTrackingConfig(epicsSys.gemsProbeGuideCmd(g))(demand.getNodChop)),
          (current.follow =!= demand.follow).option(followCmd.setFollowState(encode(demand.follow)))
        ).flattenOption

        actions.nonEmpty.option { x =>
          actions.sequence *>
            Sync[F].delay((l ^|-> GuiderConfig.tracking).set(demand)(x))
        }
      }
      else none

    val setCwfs1Guide: (VirtualGemsTelescope, NonEmptySet[Subsystem], ProbeTrackingConfig, ProbeTrackingConfig) =>
      Option[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] =
      setNgsGuide(epicsSys.cwfs1ProbeFollowCmd, EpicsTcsAoConfig.cwfs1)

    val setCwfs2Guide: (VirtualGemsTelescope, NonEmptySet[Subsystem], ProbeTrackingConfig, ProbeTrackingConfig) =>
      Option[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] =
      setNgsGuide(epicsSys.cwfs2ProbeFollowCmd, EpicsTcsAoConfig.cwfs2)

    val setCwfs3Guide: (VirtualGemsTelescope, NonEmptySet[Subsystem], ProbeTrackingConfig, ProbeTrackingConfig) =>
      Option[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] =
      setNgsGuide(epicsSys.cwfs3ProbeFollowCmd, EpicsTcsAoConfig.cwfs3)

    private def odgw1GuiderControl(g: VirtualGemsTelescope): GuideControl[F] =
      GuideControl(Subsystem.Gaos, epicsSys.odgw1ParkCmd,
        epicsSys.gemsProbeGuideCmd(g), epicsSys.odgw1FollowCmd)

    def setOdgw1Probe(g: VirtualGemsTelescope)(
      a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
    ): Option[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] =
      commonController.setGuideProbe(odgw1GuiderControl(g), (EpicsTcsAoConfig.odgw1 ^|-> GuiderConfig.tracking).set)(a, b, c)

    private def odgw2GuiderControl(g: VirtualGemsTelescope): GuideControl[F] =
      GuideControl(Subsystem.Gaos, epicsSys.odgw2ParkCmd,
        epicsSys.gemsProbeGuideCmd(g), epicsSys.odgw2FollowCmd)

    def setOdgw2Probe(g: VirtualGemsTelescope)(
      a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
    ): Option[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] =
      commonController.setGuideProbe(odgw2GuiderControl(g), (EpicsTcsAoConfig.odgw2 ^|-> GuiderConfig.tracking).set)(a, b, c)

    private def odgw3GuiderControl(g: VirtualGemsTelescope): GuideControl[F] =
      GuideControl(Subsystem.Gaos, epicsSys.odgw3ParkCmd,
        epicsSys.gemsProbeGuideCmd(g), epicsSys.odgw3FollowCmd)

    def setOdgw3Probe(g: VirtualGemsTelescope)(
      a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
    ): Option[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] =
      commonController.setGuideProbe(odgw3GuiderControl(g), (EpicsTcsAoConfig.odgw3 ^|-> GuiderConfig.tracking).set)(a, b, c)

    private def odgw4GuiderControl(g: VirtualGemsTelescope): GuideControl[F] =
      GuideControl(Subsystem.Gaos, epicsSys.odgw4ParkCmd,
        epicsSys.gemsProbeGuideCmd(g), epicsSys.odgw4FollowCmd)

    def setOdgw4Probe(g: VirtualGemsTelescope)(
      a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
    ): Option[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] =
      commonController.setGuideProbe(odgw4GuiderControl(g), (EpicsTcsAoConfig.odgw4 ^|-> GuiderConfig.tracking).set)(a, b, c)

    def setGemsProbes(subsystems: NonEmptySet[Subsystem], current: EpicsTcsAoConfig, demand: GemsGuiders)
    : List[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] = List(
        current.mapping.get(Cwfs1).flatMap(setCwfs1Guide(_, subsystems, current.cwfs1.tracking, demand.cwfs1.tracking)),
        current.mapping.get(Cwfs2).flatMap(setCwfs1Guide(_, subsystems, current.cwfs2.tracking, demand.cwfs2.tracking)),
        current.mapping.get(Cwfs3).flatMap(setCwfs1Guide(_, subsystems, current.cwfs3.tracking, demand.cwfs3.tracking)),
        current.mapping.get(Odgw1).flatMap(setOdgw1Probe(_)(subsystems, current.odgw1.tracking, demand.odgw1.tracking)),
        current.mapping.get(Odgw2).flatMap(setOdgw1Probe(_)(subsystems, current.odgw2.tracking, demand.odgw2.tracking)),
        current.mapping.get(Odgw3).flatMap(setOdgw1Probe(_)(subsystems, current.odgw3.tracking, demand.odgw3.tracking)),
        current.mapping.get(Odgw4).flatMap(setOdgw1Probe(_)(subsystems, current.odgw4.tracking, demand.odgw4.tracking))
      ).flattenOption

    // It will be a GeMS guided step only if all GeMS sources used in the base configuration are active
    private def isAoGuidedStep(baseAoCfg: GemsConfig, demand: TcsSouthAoConfig): Boolean =
      (!baseAoCfg.isCwfs1Used || demand.gds.aoguide.cwfs1.isActive) &&
        (!baseAoCfg.isCwfs2Used || demand.gds.aoguide.cwfs2.isActive) &&
        (!baseAoCfg.isCwfs3Used || demand.gds.aoguide.cwfs3.isActive) &&
        (!baseAoCfg.isOdgw1Used || demand.gds.aoguide.odgw1.isActive) &&
        (!baseAoCfg.isOdgw2Used || demand.gds.aoguide.odgw2.isActive) &&
        (!baseAoCfg.isOdgw3Used || demand.gds.aoguide.odgw3.isActive) &&
        (!baseAoCfg.isOdgw4Used || demand.gds.aoguide.odgw4.isActive) &&
        (!baseAoCfg.isOIUsed || demand.gds.oiwfs.isActive) &&
        (!baseAoCfg.isP1Used || demand.gds.pwfs1.isActive)

    // GeMS is guiding if all sources used in the GeMS base configuration are active
    private def isCurrentlyGuiding(current: EpicsTcsAoConfig, baseAoCfg: GemsConfig): Boolean =
      (!baseAoCfg.isCwfs1Used || current.cwfs1.isActive) &&
        (!baseAoCfg.isCwfs2Used || current.cwfs2.isActive) &&
        (!baseAoCfg.isCwfs3Used || current.cwfs3.isActive) &&
        (!baseAoCfg.isOdgw1Used || current.odgw1.isActive) &&
        (!baseAoCfg.isOdgw2Used || current.odgw2.isActive) &&
        (!baseAoCfg.isOdgw3Used || current.odgw3.isActive) &&
        (!baseAoCfg.isOdgw4Used || current.odgw4.isActive) &&
        (!baseAoCfg.isOIUsed || current.base.oiwfs.isActive) &&
        (!baseAoCfg.isP1Used || current.base.pwfs1.isActive)

    private def isStartingUnguidedStep(current: EpicsTcsAoConfig, baseAoCfg: GemsConfig, demand: TcsSouthAoConfig): Boolean =
      isCurrentlyGuiding(current, baseAoCfg) && !isAoGuidedStep(baseAoCfg, demand)

    private def isComingBackFromUnguidedStep(current: EpicsTcsAoConfig, baseAoCfg: GemsConfig, demand: TcsSouthAoConfig): Boolean =
      !isCurrentlyGuiding(current, baseAoCfg) && isAoGuidedStep(baseAoCfg, demand)

    private def mustPauseAoWhileOffseting(current: EpicsTcsAoConfig, demand: TcsSouthAoConfig): Boolean = {
      val distanceSquared = demand.tc.offsetA.map(_.toFocalPlaneOffset(current.base.iaa))
        .map { o => (o.x - current.base.offset.x, o.y - current.base.offset.y) }
        .map(d => d._1 * d._1 + d._2 * d._2)

      val isAnyGemsSourceUsed = (demand.gaos.isCwfs1Used && current.cwfs1.isActive) ||
        (demand.gaos.isCwfs2Used && !current.cwfs2.isActive) ||
        (demand.gaos.isCwfs3Used && !current.cwfs3.isActive) ||
        (demand.gaos.isOdgw1Used && !current.odgw1.isActive) ||
        (demand.gaos.isOdgw2Used && !current.odgw2.isActive) ||
        (demand.gaos.isOdgw3Used && !current.odgw3.isActive) ||
        (demand.gaos.isOdgw4Used && !current.odgw4.isActive)

      distanceSquared.exists(dd =>
        (isAnyGemsSourceUsed && dd > AoOffsetThreshold * AoOffsetThreshold) ||
          (demand.gaos.isP1Used && dd > pwfs1OffsetThreshold * pwfs1OffsetThreshold) ||
          (demand.gaos.isOIUsed && demand.inst.oiOffsetGuideThreshold.exists(t => dd > t * t))
      )

    }

    private def mustPauseWhileOffsetting(current: EpicsTcsAoConfig, demand: TcsSouthAoConfig): Boolean = {
      val distanceSquared = demand.tc.offsetA.map(_.toFocalPlaneOffset(current.base.iaa))
        .map { o => (o.x - current.base.offset.x, o.y - current.base.offset.y) }
        .map(d => d._1 * d._1 + d._2 * d._2)

      val becauseP1 = distanceSquared.exists(dd => Tcs.calcGuiderInUse(demand.gc, TipTiltSource.PWFS1, M1Source.PWFS1)
        && dd > pwfs1OffsetThreshold * pwfs1OffsetThreshold )

      val beacauseOi = demand.inst.oiOffsetGuideThreshold.exists(t =>
        Tcs.calcGuiderInUse(demand.gc, TipTiltSource.OIWFS, M1Source.OIWFS) && distanceSquared.exists(_ > t*t ))

      val becauseAo = demand.gc.m1Guide match {
        case M1GuideOn(M1Source.GAOS) => mustPauseAoWhileOffseting(current, demand)
        case _                        => false
      }

      beacauseOi || becauseP1 || becauseAo
    }

    def calcAoPauseConditions(current: EpicsTcsAoConfig, baseAoConfig: GemsConfig, demand: TcsSouthAoConfig): PauseConditionSet =
      PauseConditionSet.fromList(List(
        isStartingUnguidedStep(current, baseAoConfig, demand).option(PauseCondition.GaosGuideOff),
        demand.tc.offsetA.flatMap(o =>
          (!isStartingUnguidedStep(current, baseAoConfig, demand) && mustPauseAoWhileOffseting(current, demand))
            .option(PauseCondition.OffsetMove(o.toFocalPlaneOffset(current.base.iaa))))
      ).flattenOption)

    def calcAoResumeConditions(current: EpicsTcsAoConfig, baseAoConfig: GemsConfig, demand: TcsSouthAoConfig): ResumeConditionSet =
      ResumeConditionSet.fromList(List(
        isComingBackFromUnguidedStep(current, baseAoConfig, demand).option(ResumeCondition.GaosGuideOn),
        demand.tc.offsetA.flatMap(o =>
          (!isComingBackFromUnguidedStep(current, baseAoConfig, demand) && mustPauseAoWhileOffseting(current, demand))
            .option(ResumeCondition.OffsetReached(o.toFocalPlaneOffset(current.base.iaa))))
      ).flattenOption)

    def pauseResumeGaos(gaos: Gems[F], current: EpicsTcsAoConfig, baseAoConfig: GemsConfig, demand: TcsSouthAoConfig)
    : F[Gaos.PauseResume[F]] =
      gaos.pauseResume(
        calcAoPauseConditions(current, baseAoConfig, demand),
        calcAoResumeConditions(current, baseAoConfig, demand)
      )

    def calcGuideOff(current: EpicsTcsAoConfig, demand: TcsSouthAoConfig, gaosEnabled: Boolean): TcsSouthAoConfig = {
      val mustOff = mustPauseWhileOffsetting(current, demand)
      // Only turn things off here. Things that must be turned on will be turned on in GuideOn.
      def calc(c: GuiderSensorOption, d: GuiderSensorOption) = (mustOff || d === GuiderSensorOff).fold(GuiderSensorOff, c)

      (AoTcsConfig.gds[GemsGuiders, GemsConfig].modify(
        (AoGuidersConfig.pwfs1[GemsGuiders] ^<-> tagIso ^|-> GuiderConfig.detector)
          .set(calc(current.base.pwfs1.detector, demand.gds.pwfs1.detector)) >>>
          (AoGuidersConfig.oiwfs[GemsGuiders] ^<-> tagIso ^|-> GuiderConfig.detector)
            .set(calc(current.base.oiwfs.detector, demand.gds.oiwfs.detector))
      ) >>>
        AoTcsConfig.gc[GemsGuiders, GemsConfig].modify(
        TelescopeGuideConfig.mountGuide.set(
          (mustOff || demand.gc.mountGuide === MountGuideOption.MountGuideOff).fold(MountGuideOption.MountGuideOff, current.base.telescopeGuideConfig.mountGuide)
        ) >>>
          TelescopeGuideConfig.m1Guide.set(
            (mustOff || demand.gc.m1Guide === M1GuideConfig.M1GuideOff).fold(M1GuideConfig.M1GuideOff, current.base.telescopeGuideConfig.m1Guide)
          ) >>>
          TelescopeGuideConfig.m2Guide.set(
            (mustOff || demand.gc.m2Guide === M2GuideConfig.M2GuideOff).fold(M2GuideConfig.M2GuideOff, current.base.telescopeGuideConfig.m2Guide)
          )
      ) >>> normalizeM1Guiding >>> normalizeM2Guiding(gaosEnabled) >>> normalizeMountGuiding)(demand)

    }

    def guideParams(
      subsystems: NonEmptySet[Subsystem],
      current: EpicsTcsAoConfig,
      demand: TcsSouthAoConfig
    ): List[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] = List(
      commonController.setMountGuide(EpicsTcsAoConfig.base)(subsystems, current.base.telescopeGuideConfig.mountGuide, demand.gc.mountGuide),
      commonController.setM1Guide(EpicsTcsAoConfig.base)(subsystems, current.base.telescopeGuideConfig.m1Guide, demand.gc.m1Guide),
      commonController.setM2Guide(EpicsTcsAoConfig.base)(subsystems, current.base.telescopeGuideConfig.m2Guide, demand.gc.m2Guide),
      commonController.setPwfs1(EpicsTcsAoConfig.base)(subsystems, current.base.pwfs1.detector, demand.gds.pwfs1.detector),
      commonController.setOiwfs(EpicsTcsAoConfig.base)(subsystems, current.base.oiwfs.detector, demand.gds.oiwfs.detector)
    ).flattenOption


    def guideOff(
      subsystems: NonEmptySet[Subsystem],
      current: EpicsTcsAoConfig,
      demand: TcsSouthAoConfig,
      pauseGaos: Boolean
    ): F[EpicsTcsAoConfig]= {
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

    def guideOn(
      subsystems: NonEmptySet[Subsystem],
      current: EpicsTcsAoConfig,
      demand: TcsSouthAoConfig,
      gaosEnabled: Boolean
    ): F[EpicsTcsAoConfig] = {
      // If the demand turned off any WFS, normalize will turn off the corresponding processing
      val normalizedGuiding = (normalizeM1Guiding >>> normalizeM2Guiding(gaosEnabled) >>>
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

    def applyAoConfig(
      subsystems: NonEmptySet[Subsystem],
      gaos: Gems[F],
      baseAoConfig: GemsConfig,
      tcs: TcsSouthAoConfig
    ): F[Unit] = {
      def configParams(current: EpicsTcsAoConfig): List[EpicsTcsAoConfig => F[EpicsTcsAoConfig]] =
        List(
          commonController.setPwfs1Probe(EpicsTcsAoConfig.base)(subsystems, current.base.pwfs1.tracking, tcs.gds.pwfs1.tracking),
          commonController.setOiwfsProbe(EpicsTcsAoConfig.base)(subsystems, current.base.oiwfs.tracking, tcs.gds.oiwfs.tracking),
          tcs.tc.offsetA.flatMap(o => TcsControllerEpicsCommon.applyParam(subsystems.contains(Subsystem.Mount), current.base.offset,
            o.toFocalPlaneOffset(current.base.iaa), commonController.setTelescopeOffset, EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.offset
          )),
          tcs.tc.wavelA.flatMap(TcsControllerEpicsCommon.applyParam(subsystems.contains(Subsystem.Mount), current.base.wavelA, _, commonController.setWavelength,
            EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.wavelA
          )),
          commonController.setScienceFold(EpicsTcsAoConfig.base)(subsystems, current, tcs.agc.sfPos),
          commonController.setHrPickup(EpicsTcsAoConfig.base)(subsystems, current, tcs.agc)
        ).flattenOption ++ setGemsProbes(subsystems, current, tcs.gds.aoguide)

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
        s0 <- tcsConfigRetriever.retrieveConfigurationSouth(gaos.stateGetter)
        _  <- SeqexecFailure.Execution("Found useAo not set for GeMS step.").raiseError[F, Unit].whenA(!s0.base.useAo)
        pr <- pauseResumeGaos(gaos, s0, baseAoConfig, tcs)
        _  <- pr.pause.getOrElse(Applicative[F].unit)
        s1 <- guideOff(subsystems, s0, tcs, pr.pause.isEmpty)
        s2 <- sysConfig(s1)
        _  <- guideOn(subsystems, s2, tcs, pr.resume.isDefined)
        _  <- pr.resume.getOrElse(Applicative[F].unit)
      } yield ()
    }

    // Disable M1 guiding if source is off
    def normalizeM1Guiding: Endo[TcsSouthAoConfig] = cfg =>
      (AoTcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).modify{
        case g @ M1GuideConfig.M1GuideOn(src) => src match {
          case M1Source.PWFS1 => if(cfg.gds.pwfs1.isActive) g else M1GuideConfig.M1GuideOff
          case M1Source.OIWFS => if(cfg.gds.oiwfs.isActive) g else M1GuideConfig.M1GuideOff
          case _              => g
        }
        case x                => x
      }(cfg)

    // Disable M2 sources if they are off, disable M2 guiding if all are off
    def normalizeM2Guiding(gaosEnabled: Boolean): Endo[TcsSouthAoConfig] = cfg =>
      (AoTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).modify{
        case M2GuideConfig.M2GuideOn(coma, srcs) =>
          val ss = srcs.filter{
            case TipTiltSource.PWFS1 => cfg.gds.pwfs1.isActive
            case TipTiltSource.OIWFS => cfg.gds.oiwfs.isActive
            case TipTiltSource.GAOS  => gaosEnabled
            case _                   => true
          }
          if(ss.isEmpty) M2GuideConfig.M2GuideOff
          else M2GuideConfig.M2GuideOn(if(cfg.gc.m1Guide =!= M1GuideConfig.M1GuideOff) coma else ComaOption.ComaOff, ss)
        case x                     => x
      }(cfg)
  }

  // Disable Mount guiding if M2 guiding is disabled
  val normalizeMountGuiding: Endo[TcsSouthAoConfig] = cfg =>
    (AoTcsConfig.gc ^|-> TelescopeGuideConfig.mountGuide).modify{ m => (m, cfg.gc.m2Guide) match {
      case (MountGuideOption.MountGuideOn, M2GuideConfig.M2GuideOn(_, _)) => MountGuideOption.MountGuideOn
      case _                                                              => MountGuideOption.MountGuideOff
    } }(cfg)

  def apply[F[_]: Async: Logger: Timer](epicsSys: TcsEpics[F]): TcsSouthControllerEpicsAo[F] =
    new TcsSouthControllerEpicsAoImpl(epicsSys)

}
