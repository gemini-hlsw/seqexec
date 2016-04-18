package edu.gemini.seqexec.web.client.components

import japgolly.scalajs.react.{ReactComponentB, ReactNode}
import japgolly.scalajs.react.vdom.prefix_<^._

/**
  * Bar at the top of the page segtions
  */
object TextMenuSegment {
  case class Props(header: String)

  val component = ReactComponentB[Props]("TextMenuSegment")
    .stateless
    .renderPC((_, p, c) =>
      <.div(
        ^.cls := "ui top attached text menu segment",
        <.div(
          ^.cls := "ui header item",
          p.header
        ),
        c
      )
    ).build

  def apply(header: String, children: ReactNode*) = component(Props(header), children)
}
