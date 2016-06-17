package edu.gemini.seqexec.web.client.semanticui.elements.label

import japgolly.scalajs.react.{ReactComponentB, ReactNode}
import japgolly.scalajs.react.vdom.prefix_<^._

object Label {
  case class Props(text: String, htmlFor: String)

  val component = ReactComponentB[Props]("Label")
    .stateless
    .renderPC((_, p, c) =>
      <.label(
        ^.htmlFor := p.htmlFor,
        p.text,
        c
      )
    ).build

  def apply(p: Props, children: ReactNode*) = component(p, children)
}
