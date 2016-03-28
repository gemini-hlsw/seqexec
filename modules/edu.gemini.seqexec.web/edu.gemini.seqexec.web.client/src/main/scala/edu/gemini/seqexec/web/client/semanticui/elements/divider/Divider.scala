package edu.gemini.seqexec.web.client.semanticui.elements.divider

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, ReactNode}

object Divider {
  case class Props(vertical: Boolean = false,
                   horizontal: Boolean = false,
                   inverted: Boolean = false,
                   fitted: Boolean = false,
                   hidden: Boolean = false,
                   section: Boolean = false,
                   clearing: Boolean = false)

  def component = ReactComponentB[Props]("Divider")
    .renderPC((_, p, c) =>
      <.div(
        ^.cls := "ui divider",
        ^.classSet(
          "vertical"   -> p.vertical,
          "horizontal" -> p.horizontal,
          "inverted"   -> p.inverted,
          "fitted"     -> p.fitted,
          "hidden"     -> p.hidden,
          "section"    -> p.section,
          "clearing"   -> p.clearing
        ),
        c
      )
    ).build

  def apply(children: ReactNode*) = component(Props(), children: _*)
  def apply(p: Props, children: ReactNode*) = component(p, children: _*)
}
