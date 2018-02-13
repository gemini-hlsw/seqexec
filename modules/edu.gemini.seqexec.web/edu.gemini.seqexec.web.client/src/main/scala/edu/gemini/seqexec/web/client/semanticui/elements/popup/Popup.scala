// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.semanticui.elements.popup

import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.html_<^._

/**
  * Produces a dropdown menu, similar to a combobox
  */

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object Popup {
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Props(selector: String, content: String)

  private val component = ScalaComponent.builder[Props]("Popup")
    .stateless
    .renderPC((_, p, c) => <.div(c))
    .componentDidMount(ctx =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$
        import edu.gemini.web.client.facades.semanticui.SemanticUIPopup._

        println($(ctx.getDOMNode).find(ctx.props.selector).length)
        $(ctx.getDOMNode).find(ctx.props.selector).popup(
          JsPopupOptions.content(ctx.props.content)
        )
      }
    ).build

  def apply(p: Props, children: VdomNode*): Unmounted[Props, Unit, Unit] = component(p)(children: _*)
}
