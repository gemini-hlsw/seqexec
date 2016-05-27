package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactDOM}
import japgolly.scalajs.react.vdom.prefix_<^._
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.model.{SectionOpen, SectionVisibilityState}

/**
  * Created by cquiroz on 5/27/16.
  */
object LoginBox {

  case class Props(open: ModelProxy[SectionVisibilityState])

  val component = ReactComponentB[Props]("Login")
    .stateless
    .render_P(p => {println("Render" + p.open())
      <.div(
        ^.cls := "ui modal",
        <.div(
          ^.cls := "header",
          "Header"
        )
      )}
    ).componentDidUpdate(s =>
      Callback {
        import org.querki.jquery.$

        // Open the modal box
        if (s.currentProps.open() == SectionOpen) {
          $(ReactDOM.findDOMNode(s.$)).modal("show")
        }
      }
    ).build

  def apply(s: ModelProxy[SectionVisibilityState]) = component(Props(s))
}
