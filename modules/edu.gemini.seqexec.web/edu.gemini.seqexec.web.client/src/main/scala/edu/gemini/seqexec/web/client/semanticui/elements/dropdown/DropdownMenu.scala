package edu.gemini.seqexec.web.client.semanticui.dropdown

import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.semanticui.elements.menu.Item
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactDOM}
import japgolly.scalajs.react.vdom.prefix_<^._

/**
  * Produces a dropdown menu, similar to a combobox
  */
object DropdownMenu {
  case class Props(label: String,
                   defaultSelect: String,
                   items: List[String],
                   disabled: Boolean)

  def component = ReactComponentB[Props]("DropDownMenu")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "field",
        <.label(p.label),
        <.div(
          ^.cls := "ui fluid selection dropdown",
          ^.classSet(
            "disabled" -> p.disabled
          ),
          <.div(
            ^.cls := "default text",
            p.defaultSelect
          ),
          IconDropdown,
          <.input(
            ^.`type` :="hidden",
            ^.name :="iq"
          ),
          <.div(
            ^.cls := "menu",
            p.items.map(Item(_))
          )
        )
      )
    )
    .componentDidMount(s =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$
        import edu.gemini.seqexec.web.client.semanticui.SemanticUI._

        $(ReactDOM.findDOMNode(s)).find(".ui.dropdown").dropdown()
      }
    ).build

  def apply(p: Props) = component(p)
}
