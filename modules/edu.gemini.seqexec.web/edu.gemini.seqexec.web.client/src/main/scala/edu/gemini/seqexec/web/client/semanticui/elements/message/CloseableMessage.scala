// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.semanticui.elements.message

import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconClose
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.html_<^._

import scalaz.syntax.equal. _

/**
  * ReactComponent for a closeable message
  */
object CloseableMessage extends Message {

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Props(header: Option[String] = None, style: Style = Style.NotDefined)

  private def component = ScalaComponent.builder[Props]("Message")
    .stateless
    .renderPC((_, p, c) =>
      <.div(
        ^.cls := "ui message",
        ^.classSet(
          "warning"  -> (p.style === Style.Warning),
          "info"     -> (p.style === Style.Info),
          "positive" -> (p.style === Style.Positive),
          "success"  -> (p.style === Style.Success),
          "negative" -> (p.style === Style.Negative),
          "error"    -> (p.style === Style.Error)
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
        import edu.gemini.web.client.facades.semanticui.SemanticUITransition._
        import org.scalajs.dom.Element

        $(ctx.getDOMNode).on("click", (e: Element, ev: Any) =>
          $(e).closest(".message").transition("fade")
        )
      }
    )
    .build

  def apply(p: Props, children: VdomNode*): Unmounted[Props, Unit, Unit] = component(p)(children: _*)
}
