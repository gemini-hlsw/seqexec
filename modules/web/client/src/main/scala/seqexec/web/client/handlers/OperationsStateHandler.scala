// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import cats.syntax.all._
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.ModelRW
import seqexec.model.Notification.RequestFailed
import seqexec.model.enum.Resource
import seqexec.web.client.actions._
import seqexec.web.client.model.AbortOperation
import seqexec.web.client.model.CancelPauseOperation
import seqexec.web.client.model.PauseOperation
import seqexec.web.client.model.ResourceRunOperation
import seqexec.web.client.model.RunOperation
import seqexec.web.client.model.SequencesOnDisplay
import seqexec.web.client.model.StartFromOperation
import seqexec.web.client.model.StopOperation
import seqexec.web.client.model.SyncOperation
import seqexec.web.client.model.TabOperations

/**
 * Updates the state of the tabs when requests are executed
 */
class OperationsStateHandler[M](modelRW: ModelRW[M, SequencesOnDisplay])
    extends ActionHandler(modelRW)
    with Handlers[M, SequencesOnDisplay] {
  def handleRequestOperation: PartialFunction[Any, ActionResult[M]] = {
    case RequestRun(id, _) =>
      updatedL(
        SequencesOnDisplay.markOperations(
          id,
          TabOperations.runRequested.replace(RunOperation.RunInFlight)
        )
      )

    case RequestStop(id, _) =>
      updatedL(
        SequencesOnDisplay.markOperations(
          id,
          TabOperations.stopRequested.replace(StopOperation.StopInFlight)
        )
      )

    case RequestAbort(id, _) =>
      updatedL(
        SequencesOnDisplay.markOperations(
          id,
          TabOperations.abortRequested.replace(AbortOperation.AbortInFlight)
        )
      )

    case RequestSync(id) =>
      updatedL(
        SequencesOnDisplay.markOperations(
          id,
          TabOperations.syncRequested.replace(SyncOperation.SyncInFlight)
        )
      )

    case RequestPause(id) =>
      updatedL(
        SequencesOnDisplay.markOperations(
          id,
          TabOperations.pauseRequested.replace(PauseOperation.PauseInFlight)
        )
      )

    case RequestCancelPause(id) =>
      updatedL(
        SequencesOnDisplay.markOperations(
          id,
          TabOperations.cancelPauseRequested.replace(CancelPauseOperation.CancelPauseInFlight)
        )
      )

    case RunFromComplete(id, _) =>
      updatedL(
        SequencesOnDisplay.markOperations(id,
                                          TabOperations.startFromRequested
                                            .replace(StartFromOperation.StartFromIdle)
        )
      )

    case RunResourceComplete(id, s, r) =>
      updatedL(
        SequencesOnDisplay.markOperations(
          id,
          TabOperations
            .resourceRun(r)
            .replace(ResourceRunOperation.ResourceRunCompleted(s).some)
        )
      )
  }

  def handleOverrideControls: PartialFunction[Any, ActionResult[M]] = {
    case FlipSubystemsControls(id, s) =>
      updatedL(SequencesOnDisplay.changeOverrideControls(id, s))
  }

  def handleRequestResourceRun: PartialFunction[Any, ActionResult[M]] = {
    case RequestResourceRun(id, s, r) =>
      updatedL(
        SequencesOnDisplay.markOperations(
          id,
          TabOperations
            .resourceRun(r)
            .replace(ResourceRunOperation.ResourceRunInFlight(s).some)
        )
      )
  }

  def handleOperationResult: PartialFunction[Any, ActionResult[M]] = {
    case RunStarted(_) | RunStop(_) | RunGracefulStop(_) | RunAbort(_) | RunObsPause(_) |
        RunGracefulObsPause(_) | RunObsResume(_) | RunPaused(_) | RunCancelPaused(_) |
        RunResource(_, _, _) =>
      noChange

    case RunSync(id) =>
      updatedL(
        SequencesOnDisplay.markOperations(
          id,
          TabOperations.syncRequested.replace(SyncOperation.SyncIdle)
        )
      )

    case RunResourceRemote(id, s, r) =>
      // reset others instrument that may have run common resources
      val resetOthers: SequencesOnDisplay => SequencesOnDisplay =
        if (Resource.common.contains(r))
          SequencesOnDisplay.resetCommonResourceOperations(id, r)
        else
          identity
      updatedLE(
        resetOthers >>>
          SequencesOnDisplay.markOperations(
            id,
            TabOperations
              .resourceRun(r)
              .replace(ResourceRunOperation.ResourceRunInFlight(s).some)
          ),
        Effect.action(UpdateSelectedStepForce(id, s))
      )
  }

  def handleOperationFailed: PartialFunction[Any, ActionResult[M]] = {
    case RunStartFailed(id) =>
      updatedL(
        SequencesOnDisplay.markOperations(id,
                                          TabOperations.runRequested
                                            .replace(RunOperation.RunIdle)
        )
      )

    case RunSyncFailed(id) =>
      val msg          = s"Failed to sync sequence ${id.format}"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(List(msg))))
      )
      updatedLE(SequencesOnDisplay.markOperations(
                  id,
                  TabOperations.syncRequested.replace(SyncOperation.SyncIdle)
                ),
                notification
      )

    case RunAbortFailed(id) =>
      val msg          = s"Failed to abort sequence ${id.format}"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(List(msg))))
      )
      updatedLE(SequencesOnDisplay.resetOperations(id), notification)

    case RunStopFailed(id) =>
      val msg          = s"Failed to stop sequence ${id.format}"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(List(msg))))
      )
      updatedLE(SequencesOnDisplay.markOperations(
                  id,
                  TabOperations.stopRequested.replace(StopOperation.StopIdle)
                ),
                notification
      )

    case RunPauseFailed(id) =>
      val msg          = s"Failed to pause sequence ${id.format}"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(List(msg))))
      )
      updatedLE(SequencesOnDisplay.markOperations(
                  id,
                  TabOperations.pauseRequested.replace(PauseOperation.PauseIdle)
                ),
                notification
      )

    case RunFromFailed(id, sid) =>
      val msg          = s"Failed to start sequence ${id.format} from step ${sid + 1}"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(List(msg))))
      )
      updatedLE(
        SequencesOnDisplay.markOperations(
          id,
          TabOperations.startFromRequested.replace(StartFromOperation.StartFromIdle)
        ),
        notification
      )

    case RunResourceFailed(id, s, r, m) =>
      val msg          = s"Failed to configure ${r.show} for sequence ${id.format}"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(List(msg, m))))
      )
      updatedLE(SequencesOnDisplay
                  .markOperations(
                    id,
                    TabOperations
                      .resourceRun(r)
                      .replace(ResourceRunOperation.ResourceRunFailed(s).some)
                  ),
                notification
      )
  }

  def handleSelectedStep: PartialFunction[Any, ActionResult[M]] = {
    case UpdateSelectedStep(id, step) =>
      updatedSilent(value.selectStep(id, step))

    case UpdateSelectedStepForce(id, step) =>
      updated(value.selectStep(id, step))
  }

  def handleOperationComplete: PartialFunction[Any, ActionResult[M]] = {
    case RunStopCompleted(id) =>
      updatedL(SequencesOnDisplay.resetOperations(id))

    case ClearRunOnError(id) =>
      updatedL(SequencesOnDisplay.resetOperations(id))

    case ClearOperations(id) =>
      updatedL(SequencesOnDisplay.resetOperations(id))

    case ClearAllOperations =>
      updated(value.resetAllOperations)

    case ClearAllResourceOperations(id) =>
      updatedL(SequencesOnDisplay.resetAllResourceOperations(id))

    case ClearAllResourceOperationsOnStepChange(id, step) =>
      if (!value.selectedStep(id).contains(step))
        updatedL(SequencesOnDisplay.resetAllResourceOperations(id))
      else
        noChange

    case ClearResourceOperations(id, r) =>
      updatedL(SequencesOnDisplay.resetResourceOperations(id, r))
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(
      handleRequestOperation,
      handleOperationResult,
      handleOperationFailed,
      handleSelectedStep,
      handleOperationComplete,
      handleRequestResourceRun,
      handleOverrideControls
    ).combineAll
}
