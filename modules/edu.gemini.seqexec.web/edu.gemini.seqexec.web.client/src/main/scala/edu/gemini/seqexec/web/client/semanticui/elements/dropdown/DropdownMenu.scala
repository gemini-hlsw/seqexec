package edu.gemini.seqexec.web.client.semanticui.elements.dropdown

import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.semanticui.elements.menu.Item
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactComponentC, ReactComponentU, ReactDOM, TopNode}
import japgolly.scalajs.react.vdom.prefix_<^._

import scalaz.Show
import scalaz.syntax.show._
/**
  * Produces a dropdown menu, similar to a combobox
  */
object DropdownMenu {
  case class Props[A](label: String,
                   defaultSelect: A,
                   items: List[A],
                   disabled: Boolean,
                   onChange: A => Callback = (a: A) => Callback.empty)

  def component[A: Show]: ReactComponentC.ReqProps[Props[A], Unit, Unit, TopNode] = ReactComponentB[Props[A]]("DropDownMenu")
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
            p.defaultSelect.shows
          ),
          IconDropdown,
          <.input(
            ^.`type` :="hidden",
            ^.name :="iq"
          ),
          <.div(
            ^.cls := "menu",
            p.items.map(i => Item(i.shows))
          )
        )
      )
    )
    .componentDidMount(s =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$
        import edu.gemini.seqexec.web.client.semanticui.SemanticUI._

        $(ReactDOM.findDOMNode(s)).find(".ui.dropdown").dropdown(
          JsDropdownOptions
            .onChange { () =>
              println("dropdows")
            }
        )
      }
    ).build

  def apply[A: Show](p: Props[A]): ReactComponentU[Props[A], Unit, Unit, TopNode] = component[A](implicitly[Show[A]])(p)
}
