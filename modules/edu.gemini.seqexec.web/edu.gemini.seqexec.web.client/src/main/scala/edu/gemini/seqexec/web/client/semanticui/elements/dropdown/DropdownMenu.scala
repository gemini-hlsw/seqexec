package edu.gemini.seqexec.web.client.semanticui.elements.dropdown

import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.semanticui.elements.menu.Item
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactComponentC, ReactComponentU, ReactDOM, TopNode}
import japgolly.scalajs.react.vdom.prefix_<^._

import scalaz.Show
import scalaz.syntax.show._
import scalaz.syntax.equal._
import scalaz.std.string._
/**
  * Produces a dropdown menu, similar to a combobox
  */
object DropdownMenu {
  case class Props[A](label: String,
                   defaultSelect: Option[A],
                   defaultSelectText: String,
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
            p.defaultSelect.fold(p.defaultSelectText)(_.shows)
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
            .onChange { (value: String, text: String) =>
              // The text comes wrapped on react tags
              val cleanText = $(text).text()
              // We need to run the callback explicitly as we are outside the event loop
              s.props.items.find(_.shows === cleanText).map(s.props.onChange).foreach(_.runNow)
            }
        )
      }
    ).build

  def apply[A: Show](p: Props[A]): ReactComponentU[Props[A], Unit, Unit, TopNode] = component[A](implicitly[Show[A]])(p)
}
