package edu.gemini.seqexec.web.client.semanticui.elements.dropdown

import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.semanticui.elements.menu.Item
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.html_<^._

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

  private def component[A: Show] = ScalaComponent.builder[Props[A]]("DropDownMenu")
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
            ^.cls := "text",
            ^.classSet(
              "default" -> p.defaultSelect.isEmpty
            ),
            p.defaultSelect.fold(p.defaultSelectText)(_.shows)
          ),
          IconDropdown,
          <.div(
            ^.cls := "menu",
            p.items.map(i => Item(i.shows)).toTagMod
          )
        )
      )
    )
    .componentDidMount(ctx =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$
        import edu.gemini.web.client.facades.semanticui.SemanticUI._

        $(ctx.getDOMNode).find(".ui.dropdown").dropdown(
          JsDropdownOptions
            .onChange { (value: String, text: String) =>
              // The text comes wrapped on react tags
              val cleanText = $(text).text()
              // We need to run the callback explicitly as we are outside the event loop
              ctx.props.items.find(_.shows === cleanText).map(ctx.props.onChange).foreach(_.runNow)
            }
        )
      }
    ).build

  def apply[A: Show](p: Props[A]): Unmounted[Props[A], Unit, Unit] = component[A](implicitly[Show[A]])(p)
}
