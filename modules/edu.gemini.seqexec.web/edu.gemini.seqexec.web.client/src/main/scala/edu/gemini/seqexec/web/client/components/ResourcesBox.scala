// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconCheckmark
import edu.gemini.seqexec.web.client.semanticui.elements.modal.{Content, Header}
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.circuit.SeqexecCircuit
import edu.gemini.seqexec.web.client.actions.CloseResourcesBox
import japgolly.scalajs.react.component.Scala.Unmounted

/**
  * UI for the model displaying resource conflicts
  */
object ResourcesBox {

  final case class Props(visible: ModelProxy[ResourcesConflict])

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  private val component = ScalaComponent.builder[Props]("ResourcesBox")
    .stateless
    .render_P { p =>
      val ResourcesConflict(_, id) = p.visible()
      <.div(
        ^.cls := "ui tiny modal",
        Header("Resource conflict"),
        Content(
          <.p(s"There is a conflict trying to run the sequence '${id.getOrElse("")}'"),
          <.p("Possibly another sequence is being executed on the same instrument")
        ),
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
        import edu.gemini.web.client.facades.semanticui.SemanticUIModal._

        // Close the modal box if the model changes
        ctx.getDOMNode.toElement.foreach { dom =>
          ctx.currentProps.visible() match {
            case ResourcesConflict(SectionClosed, _) =>
              $(dom).modal("hide")
            case ResourcesConflict(SectionOpen, _)   =>
              // Configure the modal to autofocus and to act properly on closing
              $(dom).modal(
                JsModalOptions
                  .onHidden { () =>
                    // Need to call direct access as this is outside the event loop
                    SeqexecCircuit.dispatch(CloseResourcesBox)
                  }
              )
              // Show the modal box
              $(dom).modal("show")
          }
        }
      }
    ).build

  def apply(v: ModelProxy[ResourcesConflict]): Unmounted[Props, Unit, Unit] = component(Props(v))
}
