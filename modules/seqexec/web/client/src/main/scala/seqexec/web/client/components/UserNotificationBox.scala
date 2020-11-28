// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.syntax.all._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.colors._
import react.semanticui.elements.button.Button
import react.semanticui.modules.modal.ModalSize
import react.semanticui.modules.modal._
import seqexec.model.Notification
import seqexec.model.Notification._
import seqexec.web.client.actions.CloseUserNotificationBox
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.icons._
import seqexec.web.client.model.SectionVisibilityState._
import seqexec.web.client.model._
import seqexec.web.client.reusability._

final case class UserNotificationBox(notification: UserNotificationState)
    extends ReactProps[UserNotificationBox](UserNotificationBox.component)

/**
 * UI for the model displaying resource conflicts
 */
object UserNotificationBox {
  def header(n: Notification): String =
    n match {
      case ResourceConflict(_)    => "Resource conflict"
      case InstrumentInUse(_, _)  => "Instrument busy"
      case RequestFailed(_)       => "Request failed"
      case SubsystemBusy(_, _, _) => "Resource busy"
    }

  def body(n: Notification): List[String] =
    n match {
      case ResourceConflict(sid)         =>
        List(
          s"There is a conflict trying to run the sequence '${sid.format}'",
          "Possibly another sequence is being executed on the same instrument"
        )
      case InstrumentInUse(sid, ins)     =>
        List(
          s"Cannot select sequence '${sid.format}' for instrument '${ins.label}'",
          "Possibly another sequence is being executed on the same instrument"
        )
      case RequestFailed(msgs)           =>
        s"Request to the seqexec server failed:" :: msgs

      case SubsystemBusy(_, _, resource) =>
        List(s"Cannot configure ${resource.show}, subsystem busy")
    }

  type Props = UserNotificationBox

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.notification)

  private val close = Callback(SeqexecCircuit.dispatch(CloseUserNotificationBox))

  private val component = ScalaComponent
    .builder[Props]
    .stateless
    .render_P { p =>
      val UserNotificationState(open, not) = p.notification
      Modal(
        size = ModalSize.Tiny,
        open = open === SectionOpen,
        onClose = close
      )(
        not.map(h => ModalHeader(header(h))),
        not.map { h =>
          ModalContent(
            <.div(body(h).toTagMod(<.p(_)))
          )
        },
        ModalActions(
          Button(color = Green, positive = true, inverted = true, onClick = close)(
            IconCheckmark,
            "Ok"
          )
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

}
