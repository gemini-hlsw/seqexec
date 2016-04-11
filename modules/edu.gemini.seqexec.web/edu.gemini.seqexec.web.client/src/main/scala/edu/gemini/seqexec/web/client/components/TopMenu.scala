package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.elements.divider.Divider
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconDropdown
import edu.gemini.seqexec.web.client.semanticui.elements.menu.Item
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactDOM}
import japgolly.scalajs.react.vdom.prefix_<^._

/**
  * Menu at the top bar
  */
object TopMenu {

  val component = ReactComponentB[Unit]("SeqexecTopMenu")
    .stateless
    .render(P =>
      <.a(
        ^.href :="#",
        ^.cls := "ui right floated dropdown item",
        "Telops",
        IconDropdown,
        <.div(
          ^.cls := "menu",
          Item("Logout"),
          Divider(),
          Item("Settings")
        )
      )
    )
    .componentDidMount(s =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$

        $(ReactDOM.findDOMNode(s)).dropdown()
      }
    )
    .buildU

  def apply() = component()
}
