// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import diode.{ActionHandler, ActionResult, Effect, ModelRW, NoAction}
import seqexec.model.{ UserDetails }
import seqexec.web.client.actions._
import seqexec.web.client.services.SeqexecWebClient
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
  * Handles actions related to opening/closing the login box
  */
class UserLoginHandler[M](modelRW: ModelRW[M, Option[UserDetails]]) extends ActionHandler(modelRW) with Handlers {
  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case LoggedIn(u) =>
      // Close the login box
      val effect = Effect(Future(CloseLoginBox))
      // Close the websocket and reconnect
      val reconnect = Effect(Future(Reconnect))
      updated(Some(u), reconnect + effect)

    case Logout =>
      val effect = Effect(SeqexecWebClient.logout().map(_ => NoAction))
      val reConnect = Effect(Future(Reconnect))
      // Remove the user and call logout
      updated(None, effect + reConnect)
  }
}
