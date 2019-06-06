// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.ModelRW
import seqexec.model.events._
import seqexec.web.client.actions._
import seqexec.web.client.model.CalibrationQueues
import seqexec.web.client.components.queue.CalQueueTable

/**
  * Updates internal states when the connection opens
  */
class OpenConnectionHandler[M](modelRW: ModelRW[M, CalibrationQueues])
    extends ActionHandler(modelRW)
    with Handlers[M, CalibrationQueues] {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(ConnectionOpenEvent(u, _, _)) =>
      val ts = u
        .as(CalQueueTable.State.EditableTableState)
        .getOrElse(CalQueueTable.State.ROTableState)
      updatedL(CalibrationQueues.tableStatesT.set(ts))
  }
}
