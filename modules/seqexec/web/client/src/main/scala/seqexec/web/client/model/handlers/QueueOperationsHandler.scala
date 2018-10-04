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
import seqexec.web.client.actions.RequestAllDayCal
import seqexec.web.client.model.CalibrationQueues
import seqexec.web.client.model.QueueOperations
import seqexec.web.client.model.AddDayCalOperation
import seqexec.web.client.model.ClearAllCalOperation
import seqexec.web.client.actions._

/**
  * Updates the state of the tabs when requests are executed
  */
class QueueOperationsHandler[M](modelRW: ModelRW[M, CalibrationQueues])
    extends ActionHandler(modelRW)
    with Handlers[M, CalibrationQueues] {

  private def addDayCalL(qid: QueueId) =
    CalibrationQueues.ops ^|-> at(qid) ^<-? std.option.some ^|-> QueueOperations.addDayCalRequested

  private def clearAllCalL(qid: QueueId) =
    CalibrationQueues.ops ^|-> at(qid) ^<-? std.option.some ^|-> QueueOperations.clearAllCalRequested

  def handleAddAllDayCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestAllDayCal(qid) =>
      updatedL(addDayCalL(qid).set(AddDayCalOperation.AddDayCalInFlight))

  }

  def handleClearAllCal: PartialFunction[Any, ActionResult[M]] = {
    case RequestClearAllCal(qid) =>
      updatedL(clearAllCalL(qid).set(ClearAllCalOperation.ClearAllCalInFlight))

  }

  def handleRequestResultOk: PartialFunction[Any, ActionResult[M]] = {
    case AllDayCalCompleted(qid) =>
      updatedL(addDayCalL(qid).set(AddDayCalOperation.AddDayCalIdle))

    case ClearAllCalCompleted(qid) =>
      updatedL(clearAllCalL(qid).set(ClearAllCalOperation.ClearAllCalIdle))
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
  }
  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleAddAllDayCal,
         handleClearAllCal,
         handleRequestResultOk,
         handleRequestResultFailed).combineAll
}
