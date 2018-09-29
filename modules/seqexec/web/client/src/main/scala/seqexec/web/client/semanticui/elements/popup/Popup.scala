// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.popup

import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.raw.React

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
    .renderPC{($, _, _) =>
      // This is in principle unsafe but we are only allowing Elements on the constructor
      VdomElement($.propsChildren.only().asInstanceOf[React.Element])
    }
    .componentDidMount(ctx =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$
        import web.client.facades.semanticui.SemanticUIPopup._

        ctx.getDOMNode.toElement.foreach { dom =>
          $(dom).popup(
            JsPopupOptions.content(ctx.props.content)
          )
        }
      }
    ).build

  def apply(p: Props, children: VdomElement): Unmounted[Props, Unit, Unit] = component(p)(children)
}
