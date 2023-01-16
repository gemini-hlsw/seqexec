// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import java.util.concurrent.TimeUnit.SECONDS

import scala.concurrent.duration.FiniteDuration

import cats.effect.Async
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import monocle.macros.Lenses
import mouse.boolean._
import seqexec.server.gems.Gems._
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.gsaoi.GsaoiGuider
import seqexec.server.tcs.Gaos.PauseCondition
import seqexec.server.tcs.Gaos.PauseConditionSet
import seqexec.server.tcs.Gaos.PauseResume
import seqexec.server.tcs.Gaos.ResumeCondition
import seqexec.server.tcs.Gaos.ResumeConditionSet

class GemsControllerEpics[F[_]: Async](
  epicsSys:    GemsEpics[F],
  gsaoiGuider: GsaoiGuider[F]
)(implicit L:  Logger[F])
    extends GemsController[F] {
  import GemsControllerEpics._

  override def pauseResume(pauseReasons: PauseConditionSet, resumeReasons: ResumeConditionSet)(
    cfg:                                 GemsConfig
  ): F[PauseResume[F]] = {
    val r1 = pause(pauseReasons)
    val r2 = resume(resumeReasons)

    PauseResume(
      r1.nonEmpty.option(r1.sequence.void),
      r2.nonEmpty.option(r2.sequence.void)
    ).pure[F]
  }

  import GsaoiGuider.OdgwId._
  override val stateGetter: GemsWfsState[F] = GemsWfsState(
    epicsSys.apd1Active.map(DetectorStateOps.fromBoolean[Cwfs1DetectorState]),
    epicsSys.apd2Active.map(DetectorStateOps.fromBoolean[Cwfs2DetectorState]),
    epicsSys.apd3Active.map(DetectorStateOps.fromBoolean[Cwfs3DetectorState]),
    gsaoiGuider.currentState.map(x =>
      DetectorStateOps.fromBoolean[Odgw1DetectorState](x.isOdgwGuiding(Odgw1))
    ),
    gsaoiGuider.currentState.map(x =>
      DetectorStateOps.fromBoolean[Odgw2DetectorState](x.isOdgwGuiding(Odgw2))
    ),
    gsaoiGuider.currentState.map(x =>
      DetectorStateOps.fromBoolean[Odgw3DetectorState](x.isOdgwGuiding(Odgw3))
    ),
    gsaoiGuider.currentState.map(x =>
      DetectorStateOps.fromBoolean[Odgw4DetectorState](x.isOdgwGuiding(Odgw4))
    )
  )

  private def pause(pauseConditions: PauseConditionSet): Option[F[Unit]] = {
    val unguided = pauseConditions.contains(PauseCondition.GaosGuideOff).option(UnguidedCondition)
    val offset   = pauseConditions.offsetO.as(OffsetCondition)
    val instMove =
      pauseConditions.contains(PauseCondition.InstConfigMove).option(InstrumentCondition)

    val reasons = List(unguided, offset, instMove).flattenOption

    reasons.nonEmpty.option {
      L.debug(s"Send pause command to GeMS, reasons: $reasons") *>
        epicsSys.loopControl.setCommand(PauseCmd) *>
        epicsSys.loopControl.setReasons(reasons.mkString("|")) *>
        epicsSys.loopControl.post(CmdTimeout) *>
        L.debug("Pause command sent to GeMS")
    }

  }

  private def resume(resumeConditions: ResumeConditionSet): Option[F[Unit]] = {
    val unguided = resumeConditions.contains(ResumeCondition.GaosGuideOn).option(UnguidedCondition)
    val offset   = resumeConditions.offsetO.as(OffsetCondition)
    val instMove =
      resumeConditions.contains(ResumeCondition.InstConfigCompleted).option(InstrumentCondition)

    val reasons = List(unguided, offset, instMove).flattenOption

    reasons.nonEmpty.option {
      L.debug(s"Send resume command to GeMS, reasons: $reasons") *>
        epicsSys.loopControl.setCommand(ResumeCmd) *>
        epicsSys.loopControl.setReasons(reasons.mkString("|")) *>
        epicsSys.loopControl.post(CmdTimeout) *>
        epicsSys.waitForStableLoops(LoopStabilizationTimeout) *>
        L.debug("Resume command sent to GeMS")
    }
  }

}

object GemsControllerEpics {

  def apply[F[_]: Async: Logger](
    epicsSys:    => GemsEpics[F],
    gsaoiGuider: GsaoiGuider[F]
  ): GemsController[F] = new GemsControllerEpics[F](epicsSys, gsaoiGuider)

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

  val UnguidedCondition: String   = "Sky"
  val InstrumentCondition: String = "Filter"
  val OffsetCondition: String     = "Dither"

  val PauseCmd: String  = "PAUSE"
  val ResumeCmd: String = "RESUME"

  val CmdTimeout: FiniteDuration               = FiniteDuration(10, SECONDS)
  val LoopStabilizationTimeout: FiniteDuration = FiniteDuration(30, SECONDS)

  val ApdStart: String = "START"
  val ApdStop: String  = "STOP"

}
