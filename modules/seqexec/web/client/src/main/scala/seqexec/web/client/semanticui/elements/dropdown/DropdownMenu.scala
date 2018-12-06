// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.dropdown

import cats.Show
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.elements.menu.Item
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import cats.implicits._

/**
  * Produces a dropdown menu, similar to a combobox
  */
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object DropdownMenu {
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Props[A](label: String,
                   defaultSelect: Option[A],
                   defaultSelectText: String,
                   items: List[A],
                   disabled: Boolean,
                   onChange: A => Callback = (_: A) => Callback.empty)

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
            p.defaultSelect.fold(p.defaultSelectText)(_.show)
          ),
          IconDropdown,
          <.div(
            ^.cls := "menu",
            p.items.map(i => Item(i.show)).toTagMod
          )
        )
      )
    )
    .componentDidMount(ctx =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$
        import web.client.facades.semanticui.SemanticUIDropDown._

        ctx.getDOMNode.toElement.foreach { dom =>
          $(dom).find(".ui.dropdown").dropdown(
            JsDropdownOptions
              .onChange { (_: String, text: String) =>
                // We need to run the callback explicitly as we are outside the event loop
                ctx.props.items.find(_.show === text).map(ctx.props.onChange).foreach(_.runNow)
              }
          )
        }
      }
    ).build

  def apply[A: Show](p: Props[A]): Unmounted[Props[A], Unit, Unit] = component[A](implicitly[Show[A]])(p)
}
