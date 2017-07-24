package edu.gemini.seqexec.web.client.semanticui.elements.message

import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconClose
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.html_<^._

/**
  * ReactComponent for a closeable message
  */
object CloseableMessage extends Message {
  case class Props(header: Option[String] = None, style: Style = Style.NotDefined)

  private def component = ScalaComponent.builder[Props]("Message")
    .stateless
    .renderPC((_, p, c) =>
      <.div(
        ^.cls := "ui message",
        ^.classSet(
          "warning"  -> (p.style == Style.Warning),
          "info"     -> (p.style == Style.Info),
          "positive" -> (p.style == Style.Positive),
          "success"  -> (p.style == Style.Success),
          "negative" -> (p.style == Style.Negative),
          "error"    -> (p.style == Style.Error)
        ),
        IconClose,
        p.header.whenDefined(h =>
          <.div(
            ^.cls := "header",
            h
          )
        ),
        c
      )
    )
    .componentDidMount(ctx =>
      Callback {
        // Need to go into jQuery and semantic to enable the close button
        import org.querki.jquery.$
        import edu.gemini.web.client.facades.semanticui.SemanticUI._
        import org.scalajs.dom.Element

        $(ctx.getDOMNode).on("click", (e: Element, ev: Any) =>
          $(e).closest(".message").transition("fade")
        )
      }
    )
    .build

  def apply(p: Props, children: VdomNode*) = component(p)(children: _*)
}
