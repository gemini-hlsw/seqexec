package edu.gemini.seqexec.web.client.semanticui.elements.message

import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, ReactNode}

object IconMessage extends Message {

  def component = ReactComponentB[IconMessage.Props]("Message")
    .stateless
    .renderPC((_, p, c) =>
      <.div(
        ^.cls := "ui icon message",
        ^.classSet(
          "warning"  -> (p.style == IconMessage.Style.Warning),
          "info"     -> (p.style == IconMessage.Style.Info),
          "positive" -> (p.style == IconMessage.Style.Positive),
          "success"  -> (p.style == IconMessage.Style.Success),
          "negative" -> (p.style == IconMessage.Style.Negative),
          "error"    -> (p.style == IconMessage.Style.Error)
        ),
        p.icon,
        p.header.map(h =>
          <.div(
            ^.cls := "header",
            h
          )
        ),
        c
      )
    )
    .build

  case class Props(icon: Icon, header: Option[String] = None, style: Style = Style.NotDefined)

  def apply(p: Props, children: ReactNode*) = component(p, children: _*)
}
