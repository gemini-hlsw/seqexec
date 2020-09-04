// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.colors._
import react.semanticui.elements.button.Button
import react.semanticui.modules.modal._
import react.semanticui.modules.modal.ModalSize
import seqexec.model.Notification
import seqexec.web.client.actions.CloseUserNotificationBox
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.icons._
import seqexec.web.client.model._
import seqexec.web.client.model.SectionVisibilityState._
import seqexec.web.client.reusability._

/**
  * UI for the model displaying resource conflicts
  */
object UserNotificationBox {

  final case class Props(notification: UserNotificationState)

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.notification)

  private val close = Callback(SeqexecCircuit.dispatch(CloseUserNotificationBox))

  private val component = ScalaComponent
    .builder[Props]("UserNotificationBox")
    .stateless
    .render_P { p =>
      val UserNotificationState(open, not) = p.notification
      Modal(
        size    = ModalSize.Tiny,
        open    = open === SectionOpen,
        onClose = close
      )(
        not.map(h => ModalHeader(Notification.header(h))),
        not.map { h =>
          ModalContent(
            <.div(Notification.body(h).toTagMod(<.p(_)))
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

  def apply(v: Props): Unmounted[Props, Unit, Unit] = component(v)
}
