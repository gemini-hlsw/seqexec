// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.popup

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.raw.React
import react.common._

/**
  * Produces a popup using javascript
  * This is a barebones component waiting for the proper react component to be made available
  */

final case class Popup(selector: String, content: String) extends ReactPropsWithChildren {
  @inline def render: Seq[CtorType.ChildArg] => VdomElement = Popup.component(this)
}

object Popup {
  type Props = Popup

  private def mountPopup(component: ComponentDom.Mounted, props: Props): Callback = Callback {
    // Enable menu on Semantic UI
    import org.querki.jquery.$
    import web.client.facades.semanticui.SemanticUIPopup._

    component.toElement.foreach { dom =>
      $(dom).popup(
        JsPopupOptions.content(props.content)
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("Popup")
    .stateless
    .render_C{ children =>
      if( children.count === 1)
        VdomElement(children.only().asInstanceOf[React.Element])
      else
        <.span(children)
    }
    .componentDidMount(ctx => mountPopup(ctx.getDOMNode, ctx.props))
    .componentDidUpdate(ctx => mountPopup(ctx.getDOMNode, ctx.currentProps))
    .build
}
