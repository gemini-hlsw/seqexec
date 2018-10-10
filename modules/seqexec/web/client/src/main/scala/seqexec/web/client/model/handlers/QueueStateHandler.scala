// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import diode.ActionHandler
import diode.ActionResult
import diode.ModelRW
import seqexec.model.events._
import seqexec.model.enum.QueueManipulationOp
import seqexec.web.client.model.CalibrationQueues
import seqexec.web.client.model.RunCalOperation
import seqexec.web.client.model.StopCalOperation
import seqexec.web.client.actions._

/**
  * Handles updates to the selected sequences set
  */
class QueueStateHandler[M](modelRW: ModelRW[M, CalibrationQueues])
    extends ActionHandler(modelRW)
    with Handlers[M, CalibrationQueues] {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(QueueUpdated(QueueManipulationOp.Started(qid), _)) =>
      updatedL(CalibrationQueues.runCalL(qid).set(RunCalOperation.RunCalIdle))

    case ServerMessage(QueueUpdated(QueueManipulationOp.Stopped(qid), _)) =>
      updatedL(
        CalibrationQueues.stopCalL(qid).set(StopCalOperation.StopCalIdle))

    case ServerMessage(
        QueueUpdated(QueueManipulationOp.RemovedSeqs(qid, seqs), _)) =>
      updatedL(CalibrationQueues.removeSeqOps(qid, seqs))
  }
}
