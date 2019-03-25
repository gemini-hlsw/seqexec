// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import seqexec.web.client.model.CalibrationQueues
import seqexec.web.client.model.AddDayCalOperation
import seqexec.web.client.model.ClearAllCalOperation
import seqexec.web.client.model.RunCalOperation
import seqexec.web.client.model.StopCalOperation
import seqexec.web.client.model.RemoveSeqQueue
import seqexec.web.client.model.MoveSeqQueue
import seqexec.web.client.actions._

/**
  * Updates the state of the tabs when requests are executed
  */
class QueueOperationsHandler[M](modelRW: ModelRW[M, CalibrationQueues])
    extends ActionHandler(modelRW)
    with Handlers[M, CalibrationQueues] {

  def handleAddAllDayCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestAllSelectedSequences(qid) =>
      updatedL(
        CalibrationQueues
          .addDayCalL(qid)
          .set(AddDayCalOperation.AddDayCalInFlight))

  }

  def handleClearAllCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestClearAllCal(qid) =>
      updatedL(
        CalibrationQueues
          .clearAllCalL(qid)
          .set(ClearAllCalOperation.ClearAllCalInFlight))

  }

  def handleRunCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestRunCal(qid) =>
      updatedL(
        CalibrationQueues.calLastOpO(qid).set(none) >>>
          CalibrationQueues.runCalL(qid).set(RunCalOperation.RunCalInFlight))

  }

  def handleClearLastOp: PartialFunction[Any, ActionResult[M]] = {
    case ClearLastQueueOp(qid) =>
      updatedL(CalibrationQueues.calLastOpO(qid).set(none))

  }

  def handleStopCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestStopCal(qid) =>
      updatedL(
        CalibrationQueues.calLastOpO(qid).set(none) >>>
          CalibrationQueues.stopCalL(qid).set(StopCalOperation.StopCalInFlight))

  }

  def handleSeqOps: PartialFunction[Any, ActionResult[M]] = {
    case RequestStopCal(qid) =>
      updatedL(
        CalibrationQueues.calLastOpO(qid).set(none) >>>
          CalibrationQueues.stopCalL(qid).set(StopCalOperation.StopCalInFlight))

    case RequestRemoveSeqCal(qid, id) =>
      updatedL(
        CalibrationQueues.calLastOpO(qid).set(none) >>>
          CalibrationQueues.modifyOrAddSeqOps(
            qid,
            id,
            _.copy(removeSeqQueue = RemoveSeqQueue.RemoveSeqQueueInFlight)))

  }

  def handleRequestResultOk: PartialFunction[Any, ActionResult[M]] = {
    case AllDayCalCompleted(qid) =>
      updatedL(
        CalibrationQueues.addDayCalL(qid).set(AddDayCalOperation.AddDayCalIdle))

    case ClearAllCalCompleted(qid) =>
      updatedL(
        CalibrationQueues
          .clearAllCalL(qid)
          .set(ClearAllCalOperation.ClearAllCalIdle))

    case RunCalCompleted(_) =>
      noChange

    case StopCalCompleted(qid) =>
      updatedL(
        CalibrationQueues.stopCalL(qid).set(StopCalOperation.StopCalIdle))
  }

  def handleRequestResultFailed: PartialFunction[Any, ActionResult[M]] = {
    case AllDayCalFailed(qid) =>
      val msg = s"Failed to add all day cal sequences"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(List(msg)))))
      updatedLE(
        CalibrationQueues.addDayCalL(qid).set(AddDayCalOperation.AddDayCalIdle),
        notification)

    case ClearAllCalFailed(qid) =>
      val msg = s"Failed to clear the cal sequences"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(List(msg)))))
      updatedLE(CalibrationQueues
                  .clearAllCalL(qid)
                  .set(ClearAllCalOperation.ClearAllCalIdle),
                notification)

    case RunCalFailed(qid) =>
      val msg = s"Failed to execute the cal queue"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(List(msg)))))
      updatedLE(CalibrationQueues.runCalL(qid).set(RunCalOperation.RunCalIdle),
                notification)

    case StopCalFailed(qid) =>
      val msg = s"Failed to stop queue execution"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(List(msg)))))
      updatedLE(
        CalibrationQueues.stopCalL(qid).set(StopCalOperation.StopCalIdle),
        notification)
  }

  def handleSeqRequestResultFailed: PartialFunction[Any, ActionResult[M]] = {
    case RemoveSeqCalFailed(qid, id) =>
      val msg = s"Failed to remove sequence ${id.format} from the queue"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(List(msg)))))
      updatedLE(CalibrationQueues.modifyOrAddSeqOps(
                  qid,
                  id,
                  _.copy(removeSeqQueue = RemoveSeqQueue.RemoveSeqQueueIdle)),
                notification)

    case MoveCalFailed(qid, id) =>
      val msg = s"Failed to move sequence ${id.format} on the queue"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(List(msg)))))
      updatedLE(CalibrationQueues.modifyOrAddSeqOps(
                  qid,
                  id,
                  _.copy(moveSeqQueue = MoveSeqQueue.MoveSeqQueueIdle)),
                notification)
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(
      handleAddAllDayCal,
      handleClearAllCal,
      handleRunCal,
      handleStopCal,
      handleSeqOps,
      handleRequestResultOk,
      handleClearLastOp,
      handleSeqRequestResultFailed,
      handleRequestResultFailed
    ).combineAll
}
