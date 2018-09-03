// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.{ ActionHandler, ActionResult, Effect, ModelRW }
import seqexec.model.events.UserNotification
import seqexec.web.client.model._
import seqexec.web.client.actions._
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

class NotificationsHandler[M](modelRW: ModelRW[M, UserNotificationState]) extends ActionHandler(modelRW) with Handlers[M, UserNotificationState] {
  def handleUserNotification: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(UserNotification(not, _)) =>
      val lens = UserNotificationState.visibility.set(SectionOpen) >>> UserNotificationState.notification.set(not.some)
      val openBoxE = Effect(Future(OpenUserNotificationBox))
      updatedLE(lens, openBoxE)
  }

  def handleCloseNotification: PartialFunction[Any, ActionResult[M]] = {
    case CloseUserNotificationBox =>
      val lens = UserNotificationState.visibility.set(SectionClosed) >>> UserNotificationState.notification.set(none)
      updatedL(lens)
  }

  def handle: PartialFunction[Any, ActionResult[M]] =
    handleUserNotification
}
