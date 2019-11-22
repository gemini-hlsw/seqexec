// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.ModelRW
import seqexec.model.{NSObservationProgress, ObservationProgress, ObserveStage, Progress, Step}
import seqexec.model.enum.ActionStatus
import seqexec.model.events.ObservationProgressEvent
import seqexec.model.events.StepExecuted
import seqexec.web.client.model._
import seqexec.web.client.actions._
import seqexec.web.client.model.lenses.sequenceStepT
import seqexec.web.client.model.lenses.sequenceViewT
import squants.time.Time

import scala.concurrent.duration.Duration

/**
  * Handles updates to obs progress
  */
class ObservationsProgressStateHandler[M](
  modelRW: ModelRW[M, AllObservationsProgressState])
    extends ActionHandler(modelRW)
    with Handlers[M, AllObservationsProgressState] {

  private def adjustProgress(newProgress: Progress)(oldProgress: Progress): Progress = newProgress match {
    case nsProgress : NSObservationProgress =>
      nsProgress.stage match {
        case ObserveStage.ReadingOut =>
          oldProgress match {
            case oldNSProgress: NSObservationProgress => nsProgress.copy(remaining = Time (Duration.Zero), sub = oldNSProgress.sub)
            case _                                    => nsProgress // This would be an odd case
          }
        case ObserveStage.Idle       => oldProgress
        case ObserveStage.Preparing  =>
          oldProgress match {
            case oldNSProgress: NSObservationProgress => oldNSProgress.copy(stage = ObserveStage.Preparing)
            case oldProgress: ObservationProgress     => oldProgress.copy(stage = ObserveStage.Preparing)
          }
        case _                       => newProgress // Only advance when Acquiring.
      }
    case _                                 => newProgress
  }

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(ObservationProgressEvent(e)) =>
      updatedL(
        AllObservationsProgressState
          .progressByIdL(e.obsId, e.stepId)
          .modify(_.map(adjustProgress(e)).orElse(e.some)))

    // Remove the progress once the step completes
    case ServerMessage(e @ StepExecuted(obsId, _)) =>
      val upd =
        for {
          obs     <- sequenceViewT.find(_.id === obsId)(e)
          curSIdx <- obs.runningStep.map(_.last)
          curStep <- sequenceStepT.find(_.id === curSIdx)(obs)
        } yield
          if (Step.observeStatus.getOption(curStep).exists(_ === ActionStatus.Completed) && !curStep.isObservePaused) {
            updatedL(
              AllObservationsProgressState
                .progressByIdL(e.obsId, curSIdx)
                .set(none))
          } else {
            noChange
          }

      upd.getOrElse(noChange)
  }
}
