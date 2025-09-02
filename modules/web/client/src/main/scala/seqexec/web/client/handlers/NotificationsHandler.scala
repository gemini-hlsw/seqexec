// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import cats.syntax.all._
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.ModelRW
import seqexec.model.Notification._
import seqexec.model.events.UserNotification
import seqexec.web.client.actions._
import seqexec.web.client.model._

class NotificationsHandler[M](modelRW: ModelRW[M, UserNotificationState])
    extends ActionHandler(modelRW)
    with Handlers[M, UserNotificationState] {
  def handleUserNotification: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(UserNotification(not, _)) =>
      // Update the notification state
      val lens         = UserNotificationState.notification.replace(not.some)
      // Request opening the dialog box
      val openBoxE     = Effect(Future(OpenUserNotificationBox))
      // Update the model as load failed
      val modelUpdateE = not match {
        case InstrumentInUse(id, _)  => Effect(Future(SequenceLoadFailed(id)))
        case ResourceConflict(id)    => Effect(Future(RunStartFailed(id)))
        case SubsystemBusy(id, _, r) => Effect(Future(ClearResourceOperations(id, r)))
        case RequestFailed(_)        => VoidEffect
      }
      updatedLE(lens, openBoxE >> modelUpdateE)
  }

  def handleCloseNotification: PartialFunction[Any, ActionResult[M]] = {
    case CloseUserNotificationBox =>
      updatedL(UserNotificationState.notification.replace(none))
  }

  def handleRequestFailedNotification: PartialFunction[Any, ActionResult[M]] = {
    case RequestFailedNotification(n) =>
      val openBoxE = Effect(Future(OpenUserNotificationBox))
      updatedLE(UserNotificationState.notification.replace(n.some), openBoxE)
  }

  def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleUserNotification, handleRequestFailedNotification).combineAll
}
