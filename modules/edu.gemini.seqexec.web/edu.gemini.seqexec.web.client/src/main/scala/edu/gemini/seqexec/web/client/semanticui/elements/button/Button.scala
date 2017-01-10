package edu.gemini.seqexec.web.client.semanticui.elements.button

import edu.gemini.seqexec.web.client.semanticui.Size
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._

object Button {
  sealed trait ButtonState
  case object Active extends ButtonState
  case object Inactive extends ButtonState

  sealed trait Emphasis
  case object NoEmphasis extends Emphasis
  case object Primary extends Emphasis
  case object Secondary extends Emphasis

  sealed trait Animated
  case object NotAnimated extends Animated
  case object Animated extends Animated
  case object Vertical extends Animated
  case object Fade extends Animated

  sealed trait Type
  case object ButtonType extends Type
  case object ResetType extends Type
  case object SubmitType extends Type

  case class Props(state      : ButtonState                    = Inactive,
                   emphasis   : Emphasis                       = NoEmphasis,
                   animated   : Animated                       = NotAnimated,
                   icon       : Option[Icon]                   = None,
                   size       : Size                           = Size.NotSized,
                   buttonType : Type                           = ButtonType,
                   form       : Option[String]                 = None,
                   basic      : Boolean                        = false,
                   inverted   : Boolean                        = false,
                   circular   : Boolean                        = false,
                   labeled    : Boolean                        = false,
                   disabled   : Boolean                        = false,
                   tabIndex   : Option[Int]                    = None,
                   color      : Option[String]                 = None,
                   onClick    : Callback                       = Callback.empty,
                   dataTooltip: Option[String]                 = None,
                   extraStyles: List[scalacss.internal.StyleA] = Nil)

  def classSet(p: Props) =
    ^.classSet(
      "active"    -> (p.state == Active),
      "primary"   -> (p.emphasis == Primary),
      "secondary" -> (p.emphasis == Secondary),
      "animated"  -> (p.animated != NotAnimated),
      "vertical"  -> (p.animated == Vertical),
      "fade"      -> (p.animated == Fade),
      "icon"      -> p.icon.isDefined,
      "basic"     -> p.basic,
      "inverted"  -> p.inverted,
      "circular"  -> p.circular,
      "labeled"   -> p.labeled,
      "disabled"  -> p.disabled,
      "tiny"      -> (p.size == Size.Tiny),
      "mini"      -> (p.size == Size.Mini),
      "small"     -> (p.size == Size.Small),
      "large"     -> (p.size == Size.Large),
      "big"       -> (p.size == Size.Big),
      "huge"      -> (p.size == Size.Huge),
      "massive"   -> (p.size == Size.Massive)
    )

  def component = ReactComponentB[Props]("Button")
    .renderPC((_, p, c) =>
      if (p.animated == NotAnimated)
        <.button(
          ^.cls := "ui button",
          p.extraStyles.map(styleaToTagMod),
          ^.`type` := (p.buttonType match {
            case ButtonType => "button"
            case SubmitType => "submit"
            case ResetType  => "reset"
          }),
          p.form.map(f => formId := f),
          p.color.map(u => ^.cls := u),
          p.dataTooltip.map(t => dataTooltip := t),
          classSet(p),
          ^.onClick --> p.onClick,
          p.icon,
          c
        )
      else {
        <.div(
          ^.cls := "ui button",
          ^.tabIndex := p.tabIndex,
          classSet(p),
          ^.onClick --> p.onClick,
          p.icon,
          c
        )
      }
    ).build

  def apply(p: Props, children: ReactNode*) = component(p, children: _*)
  def apply(text: String) = component(Props(), text)
}
