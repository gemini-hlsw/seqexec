package edu.gemini.seqexec.web.client.semanticui.elements.menu

import japgolly.scalajs.react.{ReactComponentB, ReactNode}
import japgolly.scalajs.react.vdom.prefix_<^._

object Item {
  case class Props(name: String)

  val component = ReactComponentB[Props]("Item")
    .stateless
    .renderPC( (_, p, c) =>
      <.div(
        ^.cls := "item",
        p.name,
        c
      )
    ).build


  def apply(name: String, children: ReactNode*) = component(Props(name), children: _*)
}
