// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.ModelRW
import monocle.function.At.at
import monocle.std
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import seqexec.model.RequestFailed
import seqexec.model.QueueId
import seqexec.web.client.model.CalibrationQueues
import seqexec.web.client.model.QueueOperations
import seqexec.web.client.model.AddDayCalOperation
import seqexec.web.client.model.CalQueueState
import seqexec.web.client.model.ClearAllCalOperation
import seqexec.web.client.model.RunCalOperation
import seqexec.web.client.model.StopCalOperation
import seqexec.web.client.actions.RequestAllDayCal
import seqexec.web.client.actions._

/**
  * Updates the state of the tabs when requests are executed
  */
class QueueOperationsHandler[M](modelRW: ModelRW[M, CalibrationQueues])
    extends ActionHandler(modelRW)
    with Handlers[M, CalibrationQueues] {

  private def calQueueStateL(qid: QueueId) =
    CalibrationQueues.queues ^|->
      at(qid)                ^<-?
      std.option.some        ^|->
      CalQueueState.ops

  private def addDayCalL(qid: QueueId) =
    calQueueStateL(qid) ^|-> QueueOperations.addDayCalRequested

  private def clearAllCalL(qid: QueueId) =
    calQueueStateL(qid) ^|-> QueueOperations.clearAllCalRequested

  private def runCalL(qid: QueueId) =
    calQueueStateL(qid) ^|-> QueueOperations.runCalRequested

  private def stopCalL(qid: QueueId) =
    calQueueStateL(qid) ^|-> QueueOperations.stopCalRequested

  def handleAddAllDayCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestAllDayCal(qid) =>
      updatedL(addDayCalL(qid).set(AddDayCalOperation.AddDayCalInFlight))

  }

  def handleClearAllCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestClearAllCal(qid) =>
      updatedL(clearAllCalL(qid).set(ClearAllCalOperation.ClearAllCalInFlight))

  }

  def handleRunCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestRunCal(qid) =>
      updatedL(runCalL(qid).set(RunCalOperation.RunCalInFlight))

  }

  def handleStopCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestStopCal(qid) =>
      updatedL(stopCalL(qid).set(StopCalOperation.StopCalInFlight))

  }

  def handleRequestResultOk: PartialFunction[Any, ActionResult[M]] = {
    case AllDayCalCompleted(qid) =>
      updatedL(addDayCalL(qid).set(AddDayCalOperation.AddDayCalIdle))

    case ClearAllCalCompleted(qid) =>
      updatedL(clearAllCalL(qid).set(ClearAllCalOperation.ClearAllCalIdle))

    case RunCalCompleted(qid) =>
      updatedL(runCalL(qid).set(RunCalOperation.RunCalIdle))

    case StopCalCompleted(qid) =>
      updatedL(stopCalL(qid).set(StopCalOperation.StopCalIdle))
  }

  def handleRequestResultFailed: PartialFunction[Any, ActionResult[M]] = {
    case AllDayCalFailed(qid) =>
      val msg = s"Failed to add all day cal sequences"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(msg))))
      updatedLE(addDayCalL(qid).set(AddDayCalOperation.AddDayCalIdle),
                notification)

    case ClearAllCalFailed(qid) =>
      val msg = s"Failed to clear the cal sequences"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(msg))))
      updatedLE(clearAllCalL(qid).set(ClearAllCalOperation.ClearAllCalIdle),
                notification)

    case RunCalFailed(qid) =>
      val msg = s"Failed to execute the cal queue"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(msg))))
      updatedLE(runCalL(qid).set(RunCalOperation.RunCalIdle), notification)

    case StopCalFailed(qid) =>
      val msg = s"Failed to stop queue execution"
      val notification = Effect(
        Future(RequestFailedNotification(RequestFailed(msg))))
      updatedLE(stopCalL(qid).set(StopCalOperation.StopCalIdle), notification)
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleAddAllDayCal,
         handleClearAllCal,
         handleRunCal,
         handleStopCal,
         handleRequestResultOk,
         handleRequestResultFailed).combineAll
}
