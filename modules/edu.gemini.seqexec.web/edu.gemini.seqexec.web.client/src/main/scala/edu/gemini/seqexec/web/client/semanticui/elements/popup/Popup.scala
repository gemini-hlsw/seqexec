// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.semanticui.elements.popup

import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.raw.ReactElement

/**
  * Produces a popup using javascript
  * This is a barebones component waiting for the proper react component to be made available
  */
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.AsInstanceOf"))
object Popup {
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Props(selector: String, content: String)

  private val component = ScalaComponent.builder[Props]("Popup")
    .stateless
    .renderPC{($, p, c) =>
      // This is slightly unsafe though in most case we'll be passing a ReactElement here
      VdomElement($.propsChildren.only().asInstanceOf[ReactElement])
    }
    .componentDidMount(ctx =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$
        import edu.gemini.web.client.facades.semanticui.SemanticUIPopup._

        $(ctx.getDOMNode).popup(
          JsPopupOptions.content(ctx.props.content)
        )
      }
    ).build

  def apply(p: Props, children: VdomNode): Unmounted[Props, Unit, Unit] = component(p)(children)
}
