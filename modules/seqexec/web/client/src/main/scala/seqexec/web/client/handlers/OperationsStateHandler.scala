// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.ModelRW

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import seqexec.model.RequestFailed
import seqexec.web.client.model.PauseOperation
import seqexec.web.client.model.SyncOperation
import seqexec.web.client.model.RunOperation
import seqexec.web.client.model.SequencesOnDisplay
import seqexec.web.client.model.TabOperations
import seqexec.web.client.model.ResourceRunOperation
import seqexec.web.client.actions._

/**
  * Updates the state of the tabs when requests are executed
  */
class OperationsStateHandler[M](modelRW: ModelRW[M, SequencesOnDisplay])
    extends ActionHandler(modelRW)
    with Handlers[M, SequencesOnDisplay] {
  def handleRequestOperation: PartialFunction[Any, ActionResult[M]] = {
    case RequestRun(id) =>
      updated(
        value.markOperations(
          id,
          TabOperations.runRequested.set(RunOperation.RunInFlight)))

    case RequestSync(id) =>
      updated(
        value.markOperations(
          id,
          TabOperations.syncRequested.set(SyncOperation.SyncInFlight)))

    case RequestPause(id) =>
      updated(
        value.markOperations(
          id,
          TabOperations.pauseRequested.set(PauseOperation.PauseInFlight)))

    case RequestResourceRun(id, _, r) =>
      // TODO Remove this when the response comes over the WS channel
      updated(
        value.markOperations(
          id,
          TabOperations
            .resourceRun(r)
            .set(ResourceRunOperation.ResourceRunInFlight.some)))

    case RunResourceComplete(id, _, r) =>
      updated(value.markOperations(id, TabOperations.resourceRun(r).set(none)))
  }

  def handleOperationResult: PartialFunction[Any, ActionResult[M]] = {
    case RunStarted(_) =>
      noChange

    case RunStartFailed(id) =>
      updated(
        value.markOperations(
          id,
          TabOperations.runRequested.set(RunOperation.RunIdle)))

    case RunSyncFailed(id) =>
      val msg = s"Failed to sync sequence ${id.format}"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(msg))))
      updated(value.markOperations(
                id,
                TabOperations.syncRequested.set(SyncOperation.SyncIdle)),
              notification)

    case RunPauseFailed(id) =>
      val msg = s"Failed to pause sequence ${id.format}"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(msg))))
      updated(value.markOperations(
                id,
                TabOperations.pauseRequested.set(PauseOperation.PauseIdle)),
              notification)

    case RunResourceFailed(id, _, r) =>
      val msg = s"Failed to run ${r.show} for sequence ${id.format}"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(msg))))
      updated(value.markOperations(id, TabOperations.resourceRun(r).set(none)),
              notification)
  }

  def handleSelectedStep: PartialFunction[Any, ActionResult[M]] = {
    case UpdateSelectedStep(id, step) =>
      updated(value.selectStep(id, step))
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleRequestOperation,
         handleOperationResult,
         handleSelectedStep).combineAll
}
