// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import diode.{ ActionHandler, ActionResult, ModelRW }
import seqexec.web.client.actions._
import seqexec.web.client.circuit._

/**
  * Handle to preserve the steps table state
  */
class TableStateHandler[M](modelRW: ModelRW[M, TableStates]) extends ActionHandler(modelRW) with Handlers[M, TableStates] {
  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateStepsConfigTableState(state) =>
      updatedSilentL(TableStates.stepConfigTable.set(state)) // We should only do silent updates as these change too quickly

    case UpdateQueueTableState(state) =>
      updatedSilentL(TableStates.queueTable.set(state)) // We should only do silent updates as these change too quickly

    case UpdateStepTableState(id, state) =>
      updatedSilentL(TableStates.stepTableAt(id).set(Some(state))) // We should only do silent updates as these change too quickly
  }
}
