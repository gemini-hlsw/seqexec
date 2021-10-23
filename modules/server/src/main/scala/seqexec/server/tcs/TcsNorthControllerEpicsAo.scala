// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import java.time.Duration
import cats._
import cats.data._
import cats.effect.Async
import cats.effect.Timer
import cats.implicits._
import org.typelevel.log4cats.Logger
import monocle.macros.Lenses
import mouse.boolean._
import seqexec.model.M1GuideConfig
import seqexec.model.M2GuideConfig
import seqexec.model.TelescopeGuideConfig
import seqexec.model.enum.ComaOption
import seqexec.model.enum.M1Source
import seqexec.model.enum.MountGuideOption
import seqexec.model.enum.TipTiltSource
import seqexec.server.EpicsCodex.encode
import seqexec.server.SeqexecFailure
import seqexec.server.altair.Altair
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.tcs.Gaos._
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsNorthController.TcsNorthAoConfig
import shapeless.tag.@@
import squants.Length
import squants.space.Area
import squants.time.TimeConversions._
import TcsNorthController._
import seqexec.server.altair.AltairController.AltairPauseResume
import seqexec.server.tcs.TcsControllerEpicsCommon.{ calcMoveDistanceSquared, offsetNear }

trait TcsNorthControllerEpicsAo[F[_]] {
  def applyAoConfig(
    subsystems: NonEmptySet[Subsystem],
    gaos:       Altair[F],
    tcs:        TcsNorthAoConfig
  ): F[Unit]
}

object TcsNorthControllerEpicsAo {

  private final class TcsNorthControllerEpicsAoImpl[F[_]: Async: Timer](epicsSys: TcsEpics[F])(
    implicit L:                                                                   Logger[F]
  ) extends TcsNorthControllerEpicsAo[F]
      with TcsControllerEncoders {
    private val tcsConfigRetriever = TcsConfigRetriever[F](epicsSys)
    private val commonController   = TcsControllerEpicsCommon[F](epicsSys)
    private val trace              =
      Option(System.getProperty("seqexec.server.tcs.trace")).flatMap(_.toBooleanOption).isDefined

    private def setAltairProbe(
      subsystems: NonEmptySet[Subsystem],
      c:          ProbeTrackingConfig,
      d:          ProbeTrackingConfig
    ): Option[WithDebug[EpicsTcsAoConfig => F[EpicsTcsAoConfig]]] =
      if (subsystems.contains(Subsystem.Gaos)) {
        val actionList = List(
          (c.getNodChop =!= d.getNodChop)
            .option(
              commonController
                .setNodChopProbeTrackingConfig(epicsSys.pwfs2ProbeGuideCmd)(
                  d.getNodChop
                )
                .withDebug(s"NodChop(${c.getNodChop} =!= ${d.getNodChop})")
            ),
          (c.follow =!= d.follow).option(
            epicsSys.aoProbeFollowCmd
              .setFollowState(encode(d.follow))
              .withDebug(s"AoFollow(${c.follow} =!= ${d.follow})")
          )
        ).flattenOption

        val actions = actionList.reduceOption { (a, b) =>
          WithDebug(a.self *> b.self, a.debug + ", " + b.debug)
        }

        actions.map { r =>
          { (x: EpicsTcsAoConfig) =>
            r.self.as(EpicsTcsAoConfig.aowfs.set(d)(x))
          }.withDebug(s"AltairProbe(${r.debug})")
        }
      } else none

    override def applyAoConfig(
      subsystems: NonEmptySet[Subsystem],
      gaos:       Altair[F],
      tcs:        TcsNorthAoConfig
    ): F[Unit] = {
      def configParams(
        current: EpicsTcsAoConfig,
        demand:  TcsNorthAoConfig
      ): List[WithDebug[EpicsTcsAoConfig => F[EpicsTcsAoConfig]]] =
        List(
          commonController.setPwfs1Probe(EpicsTcsAoConfig.base)(subsystems,
                                                                current.base.pwfs1.tracking,
                                                                demand.gds.pwfs1.tracking
          ),
          setAltairProbe(subsystems, current.aowfs, demand.gds.aoguide.tracking),
          commonController.setOiwfsProbe(EpicsTcsAoConfig.base)(subsystems,
                                                                current.base.oiwfs.tracking,
                                                                demand.gds.oiwfs.tracking,
                                                                current.base.oiName,
                                                                demand.inst.instrument
          ),
          commonController.setScienceFold(EpicsTcsAoConfig.base)(subsystems,
                                                                 current,
                                                                 demand.agc.sfPos
          ),
          commonController.setHrPickup(EpicsTcsAoConfig.base)(subsystems, current, demand.agc)
        ).flattenOption

      def executeTargetFilterConf(filterEnabled: Boolean): F[Unit] =
        L.debug(s"${filterEnabled.fold("Activating", "Deactivating")} target filtering.") *>
          epicsSys.targetFilter.setShortCircuit(
            filterEnabled.fold(TargetFilterShortcircuitOpen, TargetFilterShortcircuitClosed)
          ) *>
          epicsSys.targetFilter.post(TcsControllerEpicsCommon.DefaultTimeout) *>
          L.debug(s"Target filtering ${filterEnabled.fold("activated", "deactivated")}.")

      def sysConfig(
        current:      EpicsTcsAoConfig,
        demand:       TcsNorthAoConfig,
        filterTarget: Boolean,
        aoConfigO:    Option[F[Unit]]
      ): F[EpicsTcsAoConfig] = {
        val mountConfigParams   =
          commonController.configMountPos(subsystems, current, demand.tc, EpicsTcsAoConfig.base)
        val paramList           = configParams(current, demand) ++ mountConfigParams
        val mountMoves: Boolean = mountConfigParams.nonEmpty
        val stabilizationTime   = demand.tc.offsetA
          .map(
            TcsSettleTimeCalculator
              .calc(current.base.instrumentOffset, _, subsystems, demand.inst.instrument)
          )
          .getOrElse(0.seconds)

        if (paramList.nonEmpty || aoConfigO.isDefined) {
          val params = paramList.foldLeft(current.pure[F]) { case (c, p) => c.flatMap(p.self) }
          val debug  = paramList.map(_.debug).mkString(", ")
          for {
            _ <- L.debug("Start TCS configuration")
            _ <- L.debug(s"TCS configuration: ${demand.show}")
            _ <- L.debug(s"for subsystems $subsystems")
            _ <- L.debug(s"TCS set because $debug").whenA(trace)
            _ <- executeTargetFilterConf(true).whenA(filterTarget && demand.tc.offsetA.isDefined)
            _ <- aoConfigO.getOrElse(Applicative[F].unit)
            s <- params
            _ <- epicsSys.post(TcsControllerEpicsCommon.ConfigTimeout)
            _ <- if (mountMoves)
                   epicsSys.waitInPosition(Duration.ofMillis(stabilizationTime.toMillis),
                                           tcsTimeout
                   ) *> L.debug("TCS inposition")
                 else if (
                   Set(Subsystem.PWFS1, Subsystem.PWFS2, Subsystem.AGUnit).exists(
                     subsystems.contains
                   )
                 )
                   epicsSys.waitAGInPosition(agTimeout) *> L.debug("AG inposition")
                 else Applicative[F].unit
            _ <- executeTargetFilterConf(false).whenA(filterTarget && demand.tc.offsetA.isDefined)
            _ <- L.debug("Completed TCS configuration")
          } yield s
        } else
          L.debug("Skipping TCS configuration") *> current.pure[F]
      }

      // A lot of decisions are taken inside the Altair object, but their effects are applied here. They are
      // communicated through the output of pauseResumeGaos
      // The outputs are:
      // - Actions to pause Altair guiding
      // - If TCS can continue guiding M1 and M2 between the pause and resume
      // - If it needs to apply a target filter
      // - If it needs to force freezing AO probe
      // - If there is a Altair configuration action to run as part of the TCS configuration (i.e. control matrix
      // calculation)
      // - Actions to resume Altair guiding
      // - If TCS can continue guiding M1 and M2 after the resume
      for {
        s0            <- tcsConfigRetriever.retrieveConfigurationNorth(gaos.isFollowing)
        _             <- SeqexecFailure
                           .Execution("Found useAo not set for AO step.")
                           .raiseError[F, Unit]
                           .whenA(!s0.base.useAo)
        pr            <- pauseResumeGaos(gaos, s0, tcs)
        adjustedDemand =
          (AoTcsConfig.gds ^|-> AoGuidersConfig
            .aoguide[GuiderConfig @@ AoGuide] ^<-> tagIso ^|-> GuiderConfig.tracking).modify(t =>
            (pr.forceFreeze && t.isActive).fold(ProbeTrackingConfig.Off, t)
          )(tcs)
        _             <- pr.pause.getOrElse(Applicative[F].unit)
        s1            <- guideOff(subsystems, s0, adjustedDemand, pr.guideWhilePaused)
        s2            <- sysConfig(
                           s1,
                           adjustedDemand,
                           pr.filterTarget,
                           pr.config
                         )
        _             <- guideOn(subsystems, s2, adjustedDemand, pr.restoreOnResume)
        _ <- pr.resume.getOrElse(Applicative[F].unit) //resume Gaos
      } yield ()
    }

    def guideParams(
      subsystems: NonEmptySet[Subsystem],
      current:    EpicsTcsAoConfig,
      demand:     TcsNorthAoConfig
    ): List[WithDebug[EpicsTcsAoConfig => F[EpicsTcsAoConfig]]] = List(
      commonController.setMountGuide(EpicsTcsAoConfig.base)(
        subsystems,
        current.base.telescopeGuideConfig.mountGuide,
        demand.gc.mountGuide
      ),
      commonController.setM1Guide(EpicsTcsAoConfig.base)(subsystems,
                                                         current.base.telescopeGuideConfig.m1Guide,
                                                         demand.gc.m1Guide
      ),
      commonController.setM2Guide(EpicsTcsAoConfig.base)(subsystems,
                                                         current.base.telescopeGuideConfig.m2Guide,
                                                         demand.gc.m2Guide
      ),
      commonController.setPwfs1(EpicsTcsAoConfig.base)(subsystems,
                                                       current.base.pwfs1.detector,
                                                       demand.gds.pwfs1.detector
      ),
      commonController.setOiwfs(EpicsTcsAoConfig.base)(subsystems,
                                                       current.base.oiwfs.detector,
                                                       demand.gds.oiwfs.detector
      )
    ).flattenOption

    private def calcGuideOffCapabilities(m2Name: TipTiltSource, m1Name: M1Source)(
      tcsGuideCurrent:                           TelescopeGuideConfig,
      guiderCurrent:                             GuiderConfig,
      tcsGuideDemand:                            TelescopeGuideConfig,
      guiderDemand:                              GuiderConfig,
      distanceSquared:                           Option[Area],
      threshold:                                 Option[Length]
    ): GuideCapabilities = {
      val canGuideWhileOffseting = (distanceSquared, threshold) match {
        case (None, _)           => true
        case (Some(_), None)     => false
        case (Some(d2), Some(t)) => d2 < t * t
      }
      val guiderActive           = guiderCurrent.isActive && guiderDemand.isActive

      GuideCapabilities(
        canGuideWhileOffseting && guiderActive && tcsGuideCurrent.m2Guide.uses(
          m2Name
        ) && tcsGuideDemand.m2Guide.uses(m2Name),
        canGuideWhileOffseting && guiderActive && tcsGuideCurrent.m1Guide.uses(
          m1Name
        ) && tcsGuideDemand.m1Guide.uses(m1Name)
      )
    }

    def calcGuideOff(
      current:             EpicsTcsAoConfig,
      demand:              TcsNorthAoConfig,
      aoGuideCapabilities: GuideCapabilities
    ): TcsNorthAoConfig = {
      val distanceSquared = calcMoveDistanceSquared(current.base, demand.tc)

      // Only turn things off here. Things that must be turned on will be turned on in guideOn.
      def calc(c: GuiderSensorOption, d: GuiderSensorOption) =
        (d === GuiderSensorOff).fold(GuiderSensorOff, c)

      val p1WhatCanGuide = calcGuideOffCapabilities(TipTiltSource.PWFS1, M1Source.PWFS1)(
        current.base.telescopeGuideConfig,
        current.base.pwfs1,
        demand.gc,
        demand.gds.pwfs1,
        distanceSquared,
        pwfs1OffsetThreshold.some
      )
      val oiWhatCanGuide = calcGuideOffCapabilities(TipTiltSource.OIWFS, M1Source.OIWFS)(
        current.base.telescopeGuideConfig,
        current.base.oiwfs,
        demand.gc,
        demand.gds.oiwfs,
        distanceSquared,
        demand.inst.oiOffsetGuideThreshold
      )
      val aoWhatCanGuide = GuideCapabilities(
        aoGuideCapabilities.canGuideM2 && current.base.telescopeGuideConfig.m2Guide
          .uses(TipTiltSource.GAOS) && demand.gc.m2Guide.uses(TipTiltSource.GAOS),
        aoGuideCapabilities.canGuideM1 && current.base.telescopeGuideConfig.m1Guide
          .uses(M1Source.GAOS) && demand.gc.m1Guide.uses(M1Source.GAOS)
      )

      val m1Enabled =
        p1WhatCanGuide.canGuideM1 || oiWhatCanGuide.canGuideM1 || aoWhatCanGuide.canGuideM1

      val m2config = demand.gc.m2Guide match {
        case M2GuideConfig.M2GuideOff               => M2GuideConfig.M2GuideOff
        case M2GuideConfig.M2GuideOn(coma, sources) =>
          val effectiveSrcs = sources.filter {
            case TipTiltSource.PWFS1 => p1WhatCanGuide.canGuideM2
            case TipTiltSource.PWFS2 => false
            case TipTiltSource.OIWFS => oiWhatCanGuide.canGuideM2
            case TipTiltSource.GAOS  => aoWhatCanGuide.canGuideM2
          }
          effectiveSrcs.isEmpty.fold(
            M2GuideConfig.M2GuideOff,
            M2GuideConfig.M2GuideOn(m1Enabled.fold(coma, ComaOption.ComaOff), effectiveSrcs)
          )
      }

      (AoTcsConfig
        .gds[GuiderConfig @@ AoGuide, AltairConfig]
        .modify(
          (AoGuidersConfig.pwfs1[GuiderConfig @@ AoGuide] ^<-> tagIso ^|-> GuiderConfig.detector)
            .set(calc(current.base.pwfs1.detector, demand.gds.pwfs1.detector)) >>>
            (AoGuidersConfig.oiwfs[GuiderConfig @@ AoGuide] ^<-> tagIso ^|-> GuiderConfig.detector)
              .set(calc(current.base.oiwfs.detector, demand.gds.oiwfs.detector))
        ) >>> m1Enabled.fold(
        identity[AoTcsConfig[GuiderConfig @@ AoGuide, AltairConfig]](_),
        (AoTcsConfig.gc[GuiderConfig @@ AoGuide, AltairConfig] ^|-> TelescopeGuideConfig.m1Guide)
          .set(M1GuideConfig.M1GuideOff)
      ) >>> (AoTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
        m2config
      ) >>> normalizeMountGuiding)(demand)
    }

    def calcAoPauseConditions(
      current: EpicsTcsAoConfig,
      demand:  TcsNorthAoConfig
    ): PauseConditionSet =
      PauseConditionSet.fromList(
        List(
          demand.tc.offsetA.flatMap { v =>
            val u = v.toFocalPlaneOffset(current.base.iaa)
            (!offsetNear(u, current.base.offset))
              .option(PauseCondition.OffsetMove(current.base.offset, u))
          },
          (!demand.gds.oiwfs.isActive).option(PauseCondition.OiOff),
          (!demand.gds.pwfs1.isActive).option(PauseCondition.P1Off),
          (!demand.gds.aoguide.isActive).option(PauseCondition.GaosGuideOff)
        ).flattenOption
      )

    def calcAoResumeConditions(
      current: EpicsTcsAoConfig,
      demand:  TcsNorthAoConfig
    ): ResumeConditionSet =
      ResumeConditionSet.fromList(
        List(
          demand.tc.offsetA.map(v =>
            ResumeCondition.OffsetReached(v.toFocalPlaneOffset(current.base.iaa))
          ),
          demand.gds.oiwfs.isActive.option(ResumeCondition.OiOn),
          demand.gds.pwfs1.isActive.option(ResumeCondition.P1On),
          demand.gds.aoguide.isActive.option(ResumeCondition.GaosGuideOn)
        ).flattenOption
      )

    def pauseResumeGaos(
      gaos:    Altair[F],
      current: EpicsTcsAoConfig,
      demand:  TcsNorthAoConfig
    ): F[AltairPauseResume[F]] =
      gaos.pauseResume(demand.gaos,
                       current.base.offset,
                       demand.inst.instrument,
                       calcAoPauseConditions(current, demand),
                       calcAoResumeConditions(current, demand)
      )

    def guideOff(
      subsystems:     NonEmptySet[Subsystem],
      current:        EpicsTcsAoConfig,
      demand:         TcsNorthAoConfig,
      aoWhatCanGuide: GuideCapabilities
    ): F[EpicsTcsAoConfig] = {
      val paramList =
        guideParams(subsystems, current, calcGuideOff(current, demand, aoWhatCanGuide))

      if (paramList.nonEmpty) {
        val params = paramList.foldLeft(current.pure[F]) { case (c, p) => c.flatMap(p.self) }
        val debug  = paramList.map(_.debug).mkString(", ")
        for {
          _ <- L.debug("Turning guide off")
          _ <- L.debug(s"guideOff set because $debug").whenA(trace)
          s <- params
          _ <- epicsSys.post(TcsControllerEpicsCommon.DefaultTimeout)
          _ <- L.debug("Guide turned off")
        } yield s
      } else
        L.debug("Skipping guide off") *> current.pure[F]
    }

    def guideOn(
      subsystems:     NonEmptySet[Subsystem],
      current:        EpicsTcsAoConfig,
      demand:         TcsNorthAoConfig,
      aoWhatCanGuide: GuideCapabilities
    ): F[EpicsTcsAoConfig] = {

      val enableM1Guide = (demand.gds.pwfs1.isActive && demand.gc.m1Guide.uses(M1Source.PWFS1)) ||
        (demand.gds.oiwfs.isActive && demand.gc.m1Guide.uses(M1Source.OIWFS)) ||
        (aoWhatCanGuide.canGuideM1 && demand.gc.m1Guide.uses(M1Source.GAOS))

      val m2config = demand.gc.m2Guide match {
        case M2GuideConfig.M2GuideOff               => M2GuideConfig.M2GuideOff
        case M2GuideConfig.M2GuideOn(coma, sources) =>
          val effectiveSrcs = sources.filter {
            case TipTiltSource.PWFS1 =>
              demand.gds.pwfs1.isActive && demand.gc.m2Guide.uses(TipTiltSource.PWFS1)
            case TipTiltSource.PWFS2 => false
            case TipTiltSource.OIWFS =>
              demand.gds.oiwfs.isActive && demand.gc.m2Guide.uses(TipTiltSource.OIWFS)
            case TipTiltSource.GAOS  =>
              aoWhatCanGuide.canGuideM2 && demand.gc.m2Guide.uses(TipTiltSource.GAOS)
          }
          effectiveSrcs.isEmpty.fold(
            M2GuideConfig.M2GuideOff,
            M2GuideConfig.M2GuideOn(enableM1Guide.fold(coma, ComaOption.ComaOff), effectiveSrcs)
          )
      }

      // If the demand turned off any WFS, normalize will turn off the corresponding processing
      val newGuideConfig = (
        enableM1Guide.fold[TcsNorthAoConfig => TcsNorthAoConfig](
          identity,
          (AoTcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).set(M1GuideConfig.M1GuideOff)
        ) >>> (AoTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).set(
          m2config
        ) >>> normalizeMountGuiding
      )(demand)

      val paramList = guideParams(subsystems, current, newGuideConfig)

      if (paramList.nonEmpty) {
        val params = paramList.foldLeft(current.pure[F]) { case (c, p) => c.flatMap(p.self) }
        val debug  = paramList.map(_.debug).mkString(", ")
        for {
          _ <- L.debug("Turning guide on")
          _ <- L.debug(s"guideOn set because $debug").whenA(trace)
          s <- params
          _ <- epicsSys.post(TcsControllerEpicsCommon.DefaultTimeout)
          _ <- L.debug("Guide turned on")
        } yield s
      } else
        L.debug("Skipping guide on") *> current.pure[F]
    }

    // Disable Mount guiding if M2 guiding is disabled
    val normalizeMountGuiding: Endo[TcsNorthAoConfig] = cfg =>
      (AoTcsConfig.gc ^|-> TelescopeGuideConfig.mountGuide).modify { m =>
        (m, cfg.gc.m2Guide) match {
          case (MountGuideOption.MountGuideOn, M2GuideConfig.M2GuideOn(_, _)) =>
            MountGuideOption.MountGuideOn
          case _                                                              => MountGuideOption.MountGuideOff
        }
      }(cfg)

  }

  def apply[F[_]: Async: Logger: Timer](epicsSys: TcsEpics[F]): TcsNorthControllerEpicsAo[F] =
    new TcsNorthControllerEpicsAoImpl(epicsSys)

  @Lenses
  final case class EpicsTcsAoConfig(
    base:  BaseEpicsTcsConfig,
    aowfs: ProbeTrackingConfig
  )

  val TargetFilterShortcircuitOpen: String   = "Open"
  val TargetFilterShortcircuitClosed: String = "Closed"

}
