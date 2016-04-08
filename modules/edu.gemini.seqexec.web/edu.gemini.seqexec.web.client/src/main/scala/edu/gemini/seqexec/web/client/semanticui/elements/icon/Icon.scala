package edu.gemini.seqexec.web.client.semanticui.elements.icon

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactNode}

/**
  * Semantic UI Icon component
  */
object Icon {
  sealed trait Flipped

  object Flipped {
    case object NotFlipped extends Flipped
    case object Horizontally extends Flipped
    case object Vertically extends Flipped
  }

  sealed trait Rotated

  object Rotated {
    case object NotRotated extends Rotated
    case object Clockwise extends Rotated
    case object CounterClockwise extends Rotated
  }

  sealed trait IconSize

  object IconSize {
    case object NotSized extends IconSize
    case object Tiny extends IconSize
    case object Mini extends IconSize
    case object Small extends IconSize
    case object Large extends IconSize
    case object Big extends IconSize
    case object Huge extends IconSize
    case object Massive extends IconSize
  }

  case class Props(id: String,
                   disabled: Boolean = false,
                   loading: Boolean = false,
                   fitted: Boolean = false,
                   size: IconSize = IconSize.NotSized,
                   link: Boolean = false,
                   flipped: Flipped = Flipped.NotFlipped,
                   rotated: Rotated = Rotated.NotRotated,
                   circular: Boolean = false,
                   bordered: Boolean = false,
                   inverted: Boolean = false,
                   color: Option[String] = None,
                   onClick: Callback = Callback.empty)

  def component = ReactComponentB[Props]("Icon")
    .stateless
    .renderPC((_, p, c) =>
      <.i(
        ^.cls := s"${p.id} icon",
        p.color.map(u => ^.cls := u),
        ^.classSet(
          "disabled"                 -> p.disabled,
          "loading"                  -> p.loading,
          "fitted"                   -> p.fitted,
          "tiny"                     -> (p.size == IconSize.Tiny),
          "mini"                     -> (p.size == IconSize.Mini),
          "small"                    -> (p.size == IconSize.Small),
          "large"                    -> (p.size == IconSize.Large),
          "big"                      -> (p.size == IconSize.Big),
          "huge"                     -> (p.size == IconSize.Huge),
          "massive"                  -> (p.size == IconSize.Massive),
          "link"                     -> p.link,
          "horizontally flipped"     -> (p.flipped == Flipped.Horizontally),
          "vertically flipped"       -> (p.flipped == Flipped.Vertically),
          "clockwise rotated"        -> (p.rotated == Rotated.Clockwise),
          "counterclockwise rotated" -> (p.rotated == Rotated.CounterClockwise),
          "circular"                 -> p.circular,
          "bordered"                 -> p.bordered,
          "inverted"                 -> p.inverted
        ),
        ^.onClick --> p.onClick,
        c
      )
    )
    .build


  def apply(id: String, children: ReactNode*) = component(Props(id), children: _*)
  def apply(p: Props, children: ReactNode*) = component(p, children: _*)
}
