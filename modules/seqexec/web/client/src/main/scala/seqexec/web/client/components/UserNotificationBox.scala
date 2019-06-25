// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.component.Scala.Unmounted
import seqexec.model.Notification
import seqexec.web.client.semanticui.elements.icon.Icon.IconCheckmark
import seqexec.web.client.semanticui.elements.modal.{Content, Header}
import seqexec.web.client.model._
import seqexec.web.client.model.SectionVisibilityState._
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.actions.CloseUserNotificationBox
import seqexec.web.client.reusability._

/**
  * UI for the model displaying resource conflicts
  */
object UserNotificationBox {

  final case class Props(notification: UserNotificationState)

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.notification)

  private val component = ScalaComponent.builder[Props]("UserNotificationBox")
    .stateless
    .render_P { p =>
      val UserNotificationState(_, not) = p.notification
      <.div(
        ^.cls := "ui tiny modal",
        not.map(h => Header(Notification.header(h))),
        not.map{h =>
          Content(
            <.div(Notification.body(h).toTagMod(<.p(_)))
          )
        },
        <.div(
          ^.cls := "actions",
          <.div(
            ^.cls := "ui green ok inverted button",
            IconCheckmark,
            "Ok"
          )
        )
      )
    }
    .componentDidUpdate(ctx =>
      Callback {
        // To properly handle the model we need to do updates with jQuery and
        // the Semantic UI javascript library
        // The calls below use a custom scala.js facade for SemanticUI
        import org.querki.jquery.$
        import web.client.facades.semanticui.SemanticUIModal._

        // Close the modal box if the model changes
        ctx.getDOMNode.toElement.foreach { dom =>
          ctx.currentProps.notification match {
            case UserNotificationState(SectionClosed, _) =>
              $(dom).modal("hide")
            case UserNotificationState(SectionOpen, _)   =>
              // Configure the modal to autofocus and to act properly on closing
              $(dom).modal(
                JsModalOptions
                  .onHidden { () =>
                    // Need to call direct access as this is outside the event loop
                    SeqexecCircuit.dispatch(CloseUserNotificationBox)
                  }
              )
              // Show the modal box
              $(dom).modal("show")
          }
        }
      }
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(v: Props): Unmounted[Props, Unit, Unit] = component(v)
}
