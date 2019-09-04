// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.ApplicativeError
import cats.effect.Async
import cats.implicits._
import monocle.macros.Lenses
import mouse.boolean._
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.gems.Gems._
import org.log4s.getLogger
import seqexec.server.gsaoi.GsaoiEpics
import seqexec.server.tcs.Gaos.{PauseCondition, PauseConditionSet, PauseResume, ResumeCondition, ResumeConditionSet}
import squants.Time
import squants.time.TimeConversions._

class GemsControllerEpics[F[_]: Async: ApplicativeError[?[_], Throwable]](epicsSys: GemsEpics[F],
                                                                          gsaoiSys: GsaoiEpics[F]
                                                                         ) extends GemsController[F] {
  import GemsControllerEpics._

  override def pauseResume(pauseReasons: PauseConditionSet, resumeReasons: ResumeConditionSet)
                          (cfg: GemsConfig)
  : F[PauseResume[F]] = retrieveConfig.map{ c =>

    // For pausing, pause loops in GeMS and then stop sensors
    val (s1, stop) = stopCwfs(c, cfg)
    val r1 = List(pause(pauseReasons), stop).flattenOption
    // And the other way around for resuming, passing s1 (the state after stopping detectors) to startTtsg
    val r2 = List(startCwfs(s1, cfg), resume(resumeReasons)).flattenOption

    PauseResume(
      r1.nonEmpty.option(r1.sequence.void),
      r2.nonEmpty.option(r2.sequence.void)
    )
  }

  private def odgwActivityState[T: DetectorStateOps](multiplier: F[Int]): F[T] =
    (gsaoiSys.guiding, multiplier).mapN{case (g, m) => g && m > 0}
      .map(DetectorStateOps.fromBoolean[T])

  override val stateGetter: GemsWfsState[F] = GemsWfsState(
    epicsSys.apd1Active.map(DetectorStateOps.fromBoolean[Cwfs1DetectorState]),
    epicsSys.apd2Active.map(DetectorStateOps.fromBoolean[Cwfs2DetectorState]),
    epicsSys.apd3Active.map(DetectorStateOps.fromBoolean[Cwfs3DetectorState]),
    odgwActivityState[Odgw1DetectorState](gsaoiSys.odgw1Multiplier),
    odgwActivityState[Odgw2DetectorState](gsaoiSys.odgw2Multiplier),
    odgwActivityState[Odgw3DetectorState](gsaoiSys.odgw3Multiplier),
    odgwActivityState[Odgw4DetectorState](gsaoiSys.odgw4Multiplier)
  )

  private val retrieveConfig: F[EpicsGems] = for{
    cwfs1 <- stateGetter.cwfs1
    cwfs2 <- stateGetter.cwfs2
    cwfs3 <- stateGetter.cwfs3
    odgw1 <- stateGetter.odgw1
    odgw2 <- stateGetter.odgw2
    odgw3 <- stateGetter.odgw3
    odgw4 <- stateGetter.odgw4
  } yield EpicsGems(
    cwfs1,
    cwfs2,
    cwfs3,
    odgw1,
    odgw2,
    odgw3,
    odgw4
  )

  private def pause(pauseConditions: PauseConditionSet)
  : Option[F[Unit]] = {
    val unguided = pauseConditions.contains(PauseCondition.GaosGuideOff).option(UnguidedCondition)
    val offset = pauseConditions.offsetO.as(OffsetCondition)
    val instMove = pauseConditions.contains(PauseCondition.InstConfigMove).option(InstrumentCondition)

    val reasons = List(unguided, offset, instMove).flattenOption

    reasons.nonEmpty.option {
      epicsSys.LoopControl.setCommand(PauseCmd) *>
        epicsSys.LoopControl.setReasons(reasons.mkString("|")) *>
        epicsSys.LoopControl.setTimeout(CmdTimeout) *>
        epicsSys.LoopControl.post.void
    }

  }

  private def resume(resumeConditions: ResumeConditionSet)
  : Option[F[Unit]] = {
    val unguided = resumeConditions.contains(ResumeCondition.GaosGuideOn).option(UnguidedCondition)
    val offset = resumeConditions.offsetO.as(OffsetCondition)
    val instMove = resumeConditions.contains(ResumeCondition.InstConfigCompleted).option(InstrumentCondition)

    val reasons = List(unguided, offset, instMove).flattenOption

    reasons.nonEmpty.option {
      epicsSys.LoopControl.setCommand(ResumeCmd) *>
        epicsSys.LoopControl.setReasons(reasons.mkString("|")) *>
        epicsSys.LoopControl.setTimeout(CmdTimeout) *>
        epicsSys.LoopControl.post *>
        epicsSys.waitForStableLoops(LoopStabilizationTimeout)
    }
  }

  import Gems.Cwfs1DetectorState.cwfs1DetectorStateDetectorStateOps
  import Gems.Cwfs2DetectorState.cwfs2DetectorStateDetectorStateOps
  import Gems.Cwfs3DetectorState.cwfs3DetectorStateDetectorStateOps

  private def stopCwfs(current: EpicsGems, demand: GemsConfig): (EpicsGems, Option[F[Unit]]) = {
    val p = List(
      (DetectorStateOps.isActive(current.cwfs1) && !demand.isCwfs1Used).option(
        (EpicsGems.cwfs1.set(Cwfs1DetectorState.Off), epicsSys.ApdControl.setApd1Cmd(ApdStop))
      ),
      (DetectorStateOps.isActive(current.cwfs2) && !demand.isCwfs2Used).option(
        (EpicsGems.cwfs2.set(Cwfs2DetectorState.Off), epicsSys.ApdControl.setApd2Cmd(ApdStop))
      ),
      (DetectorStateOps.isActive(current.cwfs3) && !demand.isCwfs3Used).option(
        (EpicsGems.cwfs3.set(Cwfs3DetectorState.Off), epicsSys.ApdControl.setApd3Cmd(ApdStop))
      )
    ).flattenOption

    (
      p.map(_._1).foldLeft(current){case (a, b) => b(a)},
      p.nonEmpty.option{
        p.map(_._2).sequence *>
        epicsSys.ApdControl.setTimeout(CmdTimeout) *>
        epicsSys.ApdControl.post.void
      }
    )
  }

  private def startCwfs(current: EpicsGems, demand: GemsConfig): Option[F[Unit]] = {
    val p = List(
      (!DetectorStateOps.isActive(current.cwfs1) && demand.isCwfs1Used).option(
        epicsSys.ApdControl.setApd1Cmd(ApdStart)
      ),
      (!DetectorStateOps.isActive(current.cwfs2) && demand.isCwfs2Used).option(
        epicsSys.ApdControl.setApd2Cmd(ApdStart)
      ),
      (!DetectorStateOps.isActive(current.cwfs3) && demand.isCwfs3Used).option(
        epicsSys.ApdControl.setApd3Cmd(ApdStart)
      )
    ).flattenOption

    p.nonEmpty.option{
      p.sequence *>
        epicsSys.ApdControl.setTimeout(CmdTimeout) *>
        epicsSys.ApdControl.post.void
    }
  }

}

object GemsControllerEpics {
  val Log = getLogger

  def apply[F[_]: Async](epicsSys: => GemsEpics[F],
                         gsaoiSys: => GsaoiEpics[F]
                        )
  : GemsController[F] = new GemsControllerEpics[F](epicsSys, gsaoiSys)

  @Lenses
  final case class EpicsGems(
                              cwfs1: Cwfs1DetectorState,
                              cwfs2: Cwfs2DetectorState,
                              cwfs3: Cwfs3DetectorState,
                              odgw1: Odgw1DetectorState,
                              odgw2: Odgw2DetectorState,
                              odgw3: Odgw3DetectorState,
                              odgw4: Odgw4DetectorState
  )

  val UnguidedCondition: String = "Sky"
  val InstrumentCondition: String = "Filter"
  val OffsetCondition: String = "Dither"

  val PauseCmd: String = "PAUSE"
  val ResumeCmd: String = "RESUME"

  val CmdTimeout: Time = 10.seconds
  val LoopStabilizationTimeout: Time = 30.seconds

  val ApdStart: String = "START"
  val ApdStop: String = "STOP"

}