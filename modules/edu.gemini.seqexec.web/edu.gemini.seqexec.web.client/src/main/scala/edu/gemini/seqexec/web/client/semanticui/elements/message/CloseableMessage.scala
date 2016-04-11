package edu.gemini.seqexec.web.client.semanticui.elements.message

import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconClose
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactDOM, ReactNode}
import japgolly.scalajs.react.vdom.prefix_<^._

/**
  * ReactComponent for a closeable message
  */
object CloseableMessage {
  sealed trait Style

  object Style {
    case object NotDefined extends Style
    case object Warning extends Style
    case object Info extends Style
    case object Positive extends Style
    case object Success extends Style
    case object Negative extends Style
    case object Error extends Style
  }

  case class Props(header: Option[String] = None, style: Style = Style.NotDefined)

  def component = ReactComponentB[Props]("Message")
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
        p.header.map(h =>
          <.div(
            ^.cls := "header",
            h
          )
        ),
        c
      )
    )
    .componentDidMount(s =>
      Callback {
        // Need to go into jQuery and semantic to enable the close button
        import org.querki.jquery.$
        import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
        import org.scalajs.dom.Element

        $(ReactDOM.findDOMNode(s)).on("click", (e: Element, ev: Any) =>
          $(e).closest(".message").transition("fade")
        )
      }
    )
    .build

  def apply(p: Props, children: ReactNode*) = component(p, children: _*)
}
