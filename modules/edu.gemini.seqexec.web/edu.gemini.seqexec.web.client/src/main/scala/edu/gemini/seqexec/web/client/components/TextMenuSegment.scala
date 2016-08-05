package edu.gemini.seqexec.web.client.components

import japgolly.scalajs.react.{ReactComponentB, ReactElement, ReactNode}
import japgolly.scalajs.react.vdom.prefix_<^._

/**
  * Bar at the top of the page segtions
  */
case class TextMenuSegment(p: TextMenuSegment.Props, children: Seq[ReactNode], key: String) {
  import TextMenuSegment.Props

  def component = ReactComponentB[Props]("TextMenuSegment")
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
    ).build.withKey(key).apply(p, children)
}

object TextMenuSegment {
  case class Props(header: String)

  // Used to call TextMenuSegment directly on a jsx component declaration
  implicit def textMenu2TagMod(i: TextMenuSegment):ReactElement = i.component

  def apply(header: String, key: String, children: ReactNode*): TextMenuSegment = TextMenuSegment(Props(header), children, key)
}
