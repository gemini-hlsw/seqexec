// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.ModelRW
import seqexec.web.client.model.SyncOperation
import seqexec.web.client.model.RunOperation
import seqexec.web.client.model.SequencesOnDisplay
import seqexec.web.client.model.TabOperations
import seqexec.web.client.actions._

/**
  * Updates the state of the tabs when requests are executed
  */
class OperationsStateHandler[M](modelRW: ModelRW[M, SequencesOnDisplay])
    extends ActionHandler(modelRW)
    with Handlers[M, SequencesOnDisplay] {
  def handleRequestOperation: PartialFunction[Any, ActionResult[M]] = {
    case RequestRun(id) =>
      updated(value.markOperations(id, TabOperations.runRequested.set(RunOperation.RunInFlight)))
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
      updated(
        value.markOperations(
          id,
          TabOperations.syncRequested.set(SyncOperation.SyncIdle)))
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleRequestOperation, handleOperationResult).combineAll
}
