// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import java.time.Duration
import cats._
import cats.data._
import cats.effect.Async
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
import seqexec.server.altair.AltairController.LgsWithOi
import seqexec.server.altair.AltairController.LgsWithP1
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

  private final class TcsNorthControllerEpicsAoImpl[F[_]: Async](epicsSys: TcsEpics[F])(implicit
    L: Logger[F]
  ) extends TcsNorthControllerEpicsAo[F]
      with TcsControllerEncoders {
    private val tcsConfigRetriever = TcsConfigRetriever[F](epicsSys)
    private val commonController   = TcsControllerEpicsCommon[F](epicsSys)
    private val trace              =
      Option(System.getProperty("seqexec.server.tcs.trace")).flatMap(_.toBooleanOption).isDefined

    private def isRelayMode(gaos: AltairConfig): Boolean = gaos match {
      case LgsWithP1 | LgsWithOi => true
      case _                     => false
    }

    // Is the AO loop guiding on this step? For Ngs and plain Lgs this is the
    // usual GuiderConfig.isActive (unchanged, verified on-sky). For the LGS
    // relay modes isActive is always false -- the AOWFS is not following and
    // its nod/chop is AllOff -- yet the AO loop is still guiding on the laser.
    // Using isActive there injected GaosGuideOff on every step (corrections
    // off + DM flattened) and GaosGuideOn never appeared on resume. tcl/tk
    // keeps the AO loop closed across offsets in lgsp1/lgsoi (RealTcs.itcl sets
    // aoduringmove unconditionally: the laser moves with the telescope), so
    // here only the step's detector demand decides. See REL-4159.
    private def aoGuideActive(gaos: AltairConfig, g: GuiderConfig): Boolean =
      if (isRelayMode(gaos)) g.detector === GuiderSensorOn else g.isActive

    private def setAltairProbe(gaos: AltairConfig)(
      subsystems: NonEmptySet[Subsystem],
      c:          ProbeTrackingConfig,
      d:          ProbeTrackingConfig
    ): Option[WithDebug[EpicsTcsAoConfig => F[EpicsTcsAoConfig]]] =
      if (subsystems.contains(Subsystem.Gaos)) {
        // The AOWFS nod/chop is written through the PWFS2 probe guide channel.
        // The demand carries whatever the Altair mode requires -- AllOff for the
        // LGS relay modes, where the TCS rejects a nod/chop set -- so the plain
        // difference check below is enough; nothing needs suppressing here.
        val actionList = List(
          (c.getNodChop =!= d.getNodChop)
            .option(
              commonController
                .setNodChopProbeTrackingConfig(epicsSys.pwfs2ProbeGuideCmd)(
                  d.getNodChop
                )
                .withDebug(s"NodChop(${c.getNodChop} =!= ${d.getNodChop})")
            ),
          // HARDWARE SAFETY -- do not remove. In LGS+P1 / LGS+OI the AOWFS has
          // no target of its own (tip-tilt comes from P1 / OI; two guiders make
          // no sense). Commanding aoFollow On there makes the Altair stage
          // follow a target that does not exist; on-sky 2026-07-28 (build with
          // 809587ce9 + 53cb1daf1) it was driven into its limit and the
          // hardware was damaged. cb4a017ae removed that demand and the next
          // test (2026-08-12) did not reproduce it. So in these modes seqexec
          // never writes aoFollow, in either direction; tcl/tk seqexec never
          // did either. This is the only aoFollow write in the code base.
          // See REL-4159.
          (!isRelayMode(gaos) && c.follow =!= d.follow).option(
            (L.debug(
              s"REL-4159 setAltairProbe: writing aoFollow ${c.follow} -> ${d.follow} (this opens/closes the AOWFS probe)"
            ) *>
              epicsSys.aoProbeFollowCmd
                .setFollowState(encode(d.follow)))
              .withDebug(s"AoFollow(${c.follow} =!= ${d.follow})")
          )
        ).flattenOption

        val actions = actionList.reduceOption { (a, b) =>
          WithDebug(a.self *> b.self, a.debug + ", " + b.debug)
        }

        actions.map { r =>
          { (x: EpicsTcsAoConfig) =>
            r.self.as(EpicsTcsAoConfig.aowfs.replace(d)(x))
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
          setAltairProbe(demand.gaos)(subsystems, current.aowfs, demand.gds.aoguide.tracking),
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
        current:           EpicsTcsAoConfig,
        demand:            TcsNorthAoConfig,
        pauseTargetFilter: Boolean,
        aoConfigO:         Option[F[Unit]]
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
            _ <- executeTargetFilterConf(false).whenA(pauseTargetFilter && mountMoves)
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
            _ <- executeTargetFilterConf(true).whenA(pauseTargetFilter && mountMoves)
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
          AoTcsConfig.gds
            .andThen(AoGuidersConfig.aoguide[GuiderConfig @@ AoGuide])
            .andThen(tagIso[GuiderConfig, AoGuide])
            .andThen(GuiderConfig.tracking)
            .modify(t => (pr.forceFreeze && t.isActive).fold(ProbeTrackingConfig.Off, t))(tcs)
        _             <- pr.pause.getOrElse(Applicative[F].unit)
        s1            <- guideOff(subsystems, s0, adjustedDemand, pr.guideWhilePaused)
        s2            <- sysConfig(
                           s1,
                           adjustedDemand,
                           pr.pauseTargetFilter,
                           pr.config
                         )
        _             <- guideOn(subsystems, s2, adjustedDemand, pr.restoreOnResume)
        _             <- pr.resume.getOrElse(Applicative[F].unit) // resume Gaos
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

    // Can this guider keep guiding through the move: active before and after,
    // and the move is within its offset threshold.
    private def guiderKeepsGuiding(
      guiderCurrent:   GuiderConfig,
      guiderDemand:    GuiderConfig,
      distanceSquared: Option[Area],
      threshold:       Option[Length]
    ): Boolean = {
      val canGuideWhileOffseting = (distanceSquared, threshold) match {
        case (None, _)           => true
        case (Some(_), None)     => false
        case (Some(d2), Some(t)) => d2 < t * t
      }
      canGuideWhileOffseting && guiderCurrent.isActive && guiderDemand.isActive
    }

    private def calcGuideOffCapabilities(m2Name: TipTiltSource, m1Name: M1Source)(
      tcsGuideCurrent: TelescopeGuideConfig,
      guiderCurrent:   GuiderConfig,
      tcsGuideDemand:  TelescopeGuideConfig,
      guiderDemand:    GuiderConfig,
      distanceSquared: Option[Area],
      threshold:       Option[Length]
    ): GuideCapabilities = {
      val canGuide = guiderKeepsGuiding(guiderCurrent, guiderDemand, distanceSquared, threshold)

      GuideCapabilities(
        canGuide && tcsGuideCurrent.m2Guide.uses(
          m2Name
        ) && tcsGuideDemand.m2Guide.uses(m2Name),
        canGuide && tcsGuideCurrent.m1Guide.uses(
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

      val p1WhatCanGuide      = calcGuideOffCapabilities(TipTiltSource.PWFS1, M1Source.PWFS1)(
        current.base.telescopeGuideConfig,
        current.base.pwfs1,
        demand.gc,
        demand.gds.pwfs1,
        distanceSquared,
        pwfs1OffsetThreshold.some
      )
      val oiWhatCanGuide      = calcGuideOffCapabilities(TipTiltSource.OIWFS, M1Source.OIWFS)(
        current.base.telescopeGuideConfig,
        current.base.oiwfs,
        demand.gc,
        demand.gds.oiwfs,
        distanceSquared,
        demand.inst.oiOffsetGuideThreshold
      )
      // LGS relay modes, as tcl/tk seqexec does in RealTcs.itcl move: if the
      // tip-tilt guider (P1 / OI) cannot keep guiding through this move, all M2
      // guiding goes off (GAOS included), mount guiding with it, and the
      // tip-tilt guider stops integrating; M1 is left alone. guideOn restores
      // them afterwards. See REL-4159.
      val relayTtKeepsGuiding = demand.gaos match {
        case LgsWithP1 =>
          guiderKeepsGuiding(current.base.pwfs1,
                             demand.gds.pwfs1,
                             distanceSquared,
                             pwfs1OffsetThreshold.some
          ).some
        case LgsWithOi =>
          guiderKeepsGuiding(current.base.oiwfs,
                             demand.gds.oiwfs,
                             distanceSquared,
                             demand.inst.oiOffsetGuideThreshold
          ).some
        case _         => none
      }
      val stopTtGuider        = relayTtKeepsGuiding.contains(false)

      val aoWhatCanGuide = GuideCapabilities(
        !stopTtGuider && aoGuideCapabilities.canGuideM2 && current.base.telescopeGuideConfig.m2Guide
          .uses(TipTiltSource.GAOS) && demand.gc.m2Guide.uses(TipTiltSource.GAOS),
        aoGuideCapabilities.canGuideM1 && current.base.telescopeGuideConfig.m1Guide
          .uses(M1Source.GAOS) && demand.gc.m1Guide.uses(M1Source.GAOS)
      )

      val m1Enabled = relayTtKeepsGuiding.isDefined ||
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
          AoGuidersConfig
            .pwfs1[GuiderConfig @@ AoGuide]
            .andThen(tagIso[GuiderConfig, P1Config])
            .andThen(GuiderConfig.detector)
            .replace(
              (stopTtGuider && demand.gaos === LgsWithP1)
                .fold(GuiderSensorOff, calc(current.base.pwfs1.detector, demand.gds.pwfs1.detector))
            ) >>>
            AoGuidersConfig
              .oiwfs[GuiderConfig @@ AoGuide]
              .andThen(tagIso[GuiderConfig, OIConfig])
              .andThen(GuiderConfig.detector)
              .replace(
                (stopTtGuider && demand.gaos === LgsWithOi)
                  .fold(GuiderSensorOff,
                        calc(current.base.oiwfs.detector, demand.gds.oiwfs.detector)
                  )
              )
        ) >>> m1Enabled.fold(
        identity[AoTcsConfig[GuiderConfig @@ AoGuide, AltairConfig]](_),
        AoTcsConfig
          .gc[GuiderConfig @@ AoGuide, AltairConfig]
          .andThen(TelescopeGuideConfig.m1Guide)
          .replace(M1GuideConfig.M1GuideOff)
      ) >>> AoTcsConfig.gc
        .andThen(TelescopeGuideConfig.m2Guide)
        .replace(
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
          (!aoGuideActive(demand.gaos, demand.gds.aoguide)).option(PauseCondition.GaosGuideOff)
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
          aoGuideActive(demand.gaos, demand.gds.aoguide).option(ResumeCondition.GaosGuideOn)
        ).flattenOption
      )

    def pauseResumeGaos(
      gaos:    Altair[F],
      current: EpicsTcsAoConfig,
      demand:  TcsNorthAoConfig
    ): F[AltairPauseResume[F]] = {
      val pauseConds  = calcAoPauseConditions(current, demand)
      val resumeConds = calcAoResumeConditions(current, demand)
      L.debug(
        s"REL-4159 pauseResumeGaos: gaos=${demand.gaos}, " +
          s"aoGuideActive=${aoGuideActive(demand.gaos, demand.gds.aoguide)} " +
          s"(follow=${demand.gds.aoguide.tracking.follow}, " +
          s"nodChop=${demand.gds.aoguide.tracking.getNodChop}, " +
          s"detector=${demand.gds.aoguide.detector}), " +
          s"aowfs.follow(current)=${current.aowfs.follow}, " +
          s"=> pauseConds=$pauseConds, resumeConds=$resumeConds"
      ) *>
        gaos.pauseResume(demand.gaos,
                         current.base.offset,
                         demand.inst.instrument,
                         pauseConds,
                         resumeConds
        )
    }

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
          AoTcsConfig.gc.andThen(TelescopeGuideConfig.m1Guide).replace(M1GuideConfig.M1GuideOff)
        ) >>> AoTcsConfig.gc
          .andThen(TelescopeGuideConfig.m2Guide)
          .replace(
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
      AoTcsConfig.gc
        .andThen(TelescopeGuideConfig.mountGuide)
        .modify { m =>
          (m, cfg.gc.m2Guide) match {
            case (MountGuideOption.MountGuideOn, M2GuideConfig.M2GuideOn(_, _)) =>
              MountGuideOption.MountGuideOn
            case _                                                              => MountGuideOption.MountGuideOff
          }
        }(cfg)

  }

  def apply[F[_]: Async: Logger](epicsSys: TcsEpics[F]): TcsNorthControllerEpicsAo[F] =
    new TcsNorthControllerEpicsAoImpl(epicsSys)

  @Lenses
  final case class EpicsTcsAoConfig(
    base:  BaseEpicsTcsConfig,
    aowfs: ProbeTrackingConfig
  )

  val TargetFilterShortcircuitOpen: String   = "Open"
  val TargetFilterShortcircuitClosed: String = "Closed"

}
