// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import diode.ActionHandler
import diode.ActionResult
import diode.ModelRW
import seqexec.web.client.actions._
import seqexec.web.client.model.AppTableStates

/**
  * Handle to preserve the table states
  */
class TableStateHandler[M](modelRW: ModelRW[M, AppTableStates])
    extends ActionHandler(modelRW)
    with Handlers[M, AppTableStates] {
  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateStepsConfigTableState(state) =>
      updatedSilentL(AppTableStates.stepConfigTable.set(state)) // We should only do silent updates as these change too quickly

    case UpdateSessionQueueTableState(state) =>
      updatedSilentL(AppTableStates.sessionQueueTable.set(state)) // We should only do silent updates as these change too quickly

    case UpdateStepTableState(id, state) =>
      updatedSilentL(AppTableStates.stepsTableAtL(id).set(Some(state))) // We should only do silent updates as these change too quickly

    case UpdateCalTableState(id, state) =>
      updatedSilentL(AppTableStates.queueTableAtL(id).set(Some(state))) // We should only do silent updates as these change too quickly
  }
}
