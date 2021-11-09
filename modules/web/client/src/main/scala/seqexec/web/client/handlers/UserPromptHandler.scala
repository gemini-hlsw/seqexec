// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import cats.syntax.all._
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.ModelRW
import seqexec.model.UserPrompt
import seqexec.model.events.UserPromptNotification
import seqexec.web.client.actions._
import seqexec.web.client.model._
import seqexec.web.client.circuit.UserPromptFocus
import seqexec.model.Observer

class UserPromptHandler[M](modelRW: ModelRW[M, UserPromptFocus])
    extends ActionHandler(modelRW)
    with Handlers[M, UserPromptFocus] {
  val lens                                                          = UserPromptFocus.user ^|-> UserPromptState.notification
  def handleUserNotification: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(UserPromptNotification(not, _)) =>
      // Update the model as load failed
      val modelUpdateE = not match {
        case UserPrompt.ChecksOverride(id, _, _) => Effect(Future(RunStartFailed(id)))
      }
      updatedLE(lens.set(not.some), modelUpdateE)
  }

  def handleClosePrompt: PartialFunction[Any, ActionResult[M]] = { case CloseUserPromptBox(x) =>
    val overrideEffect = (this.value.user.notification, this.value.displayName) match {
      case (Some(UserPrompt.ChecksOverride(id, stp, _)), Some(dn))
          if x === UserPromptResult.Cancel =>
        Effect(Future(RequestRunFrom(id, Observer(dn), stp, RunOptions.ChecksOverride)))
      case _ => VoidEffect
    }
    updatedLE(lens.set(none), overrideEffect)
  }

  def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleUserNotification, handleClosePrompt).combineAll
}
