package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.divider.Divider
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconDropdown
import edu.gemini.seqexec.web.client.semanticui.elements.menu.Item
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactDOM}
import japgolly.scalajs.react.vdom.prefix_<^._

/**
  * Menu at the top bar
  */
object TopMenu {

  case class Props(u: Option[UserDetails])

  val loginButton = Button(Button.Props(emphasis = Button.Secondary), "Login")

  val component = ReactComponentB[Props]("SeqexecTopMenu")
    .stateless
    .render_P(p =>
      <.a(
        ^.href :="#",
        ^.cls := "ui right floated dropdown item",
        p.u.map(i => <.span(i.displayName)).getOrElse(loginButton),
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
    .build

  def apply(u: ModelProxy[Option[UserDetails]]) = component(Props(u()))
}
