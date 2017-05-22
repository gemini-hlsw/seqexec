package edu.gemini.seqexec.web.client.semanticui.elements.label

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._

object Label {
  case class Props(text: String, htmlFor: String)

  val component = ScalaComponent.builder[Props]("Label")
    .stateless
    .renderPC((_, p, c) =>
      <.label(
        ^.htmlFor := p.htmlFor,
        p.text,
        c
      )
    ).build

  def apply(p: Props, children: VdomNode*) = component(p)(children: _*)
}
