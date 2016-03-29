package edu.gemini.seqexec.web.client.semanticui.elements.icon

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, ReactNode}

/**
  * Semantic UI Icon component
  */
object Icon {
  sealed trait Flipped
  case object NotFlipped extends Flipped
  case object Horizontally extends Flipped
  case object Vertically extends Flipped

  sealed trait Rotated
  case object NotRotated extends Rotated
  case object Clockwise extends Rotated
  case object CounterClockwise extends Rotated

  case class Props(id: String,
                   disabled: Boolean = false,
                   loading: Boolean = false,
                   fitted: Boolean = false,
                   tiny: Boolean = false,
                   mini: Boolean = false,
                   small: Boolean = false,
                   large: Boolean = false,
                   big: Boolean = false,
                   huge: Boolean = false,
                   massive: Boolean = false,
                   link: Boolean = false,
                   flipped: Flipped = NotFlipped,
                   rotated: Rotated = NotRotated,
                   circular: Boolean = false,
                   bordered: Boolean = false,
                   inverted: Boolean = false,
                   color: Option[String] = None)

  def component = ReactComponentB[Props]("Icon")
    .stateless
    .renderPC((_, p, c) =>
      <.i(
        ^.cls := s"${p.id} icon",
        p.color.map(u => ^.cls := u),
        ^.classSet(
          "disabled" -> p.disabled,
          "loading" -> p.loading,
          "fitted" -> p.fitted,
          "mini" -> p.mini,
          "tiny" -> p.tiny,
          "small" -> p.small,
          "large" -> p.large,
          "big" -> p.big,
          "huge" -> p.huge,
          "massive" -> p.massive,
          "link" -> p.link,
          "horizontally flipped" -> (p.flipped == Horizontally),
          "vertically flipped" -> (p.flipped == Vertically),
          "clockwise rotated" -> (p.rotated == Clockwise),
          "counterclockwise rotated" -> (p.rotated == CounterClockwise),
          "circular" -> p.circular,
          "bordered" -> p.bordered,
          "inverted" -> p.inverted
        ),
        c
      )
    )
    .build


  def apply(id: String, children: ReactNode*) = component(Props(id), children: _*)
  def apply(p: Props, children: ReactNode*) = component(p, children: _*)
}
