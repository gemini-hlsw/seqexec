// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.button

import cats.Eq
import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import react.common.style._
import react.common.implicits._
import seqexec.web.client.semanticui.Size
import seqexec.web.client.semanticui._
import seqexec.web.client.semanticui.elements.icon.Icon
import scala.scalajs.js

object Button {
  sealed trait ButtonState
  case object Active   extends ButtonState
  case object Inactive extends ButtonState

  object ButtonState {
    implicit val equal: Eq[ButtonState] = Eq.fromUniversalEquals
  }

  sealed trait Emphasis
  case object NoEmphasis extends Emphasis
  case object Primary    extends Emphasis
  case object Secondary  extends Emphasis

  object Emphasis {
    implicit val equal: Eq[Emphasis] = Eq.fromUniversalEquals
  }

  sealed trait Animated
  case object NotAnimated    extends Animated
  case object AnimatedButton extends Animated
  case object Vertical       extends Animated
  case object Fade           extends Animated

  object Animated {
    implicit val equal: Eq[Animated] = Eq.fromUniversalEquals
  }

  sealed trait Type
  case object ButtonType extends Type
  case object ResetType  extends Type
  case object SubmitType extends Type

  sealed trait Labeled
  case object NotLabeled   extends Labeled
  case object LeftLabeled  extends Labeled
  case object RightLabeled extends Labeled

  object Labeled {
    implicit val equal: Eq[Labeled] = Eq.fromUniversalEquals
  }

  object Type {
    implicit val equal: Eq[Type] = Eq.fromUniversalEquals
  }

  final case class Props(state: ButtonState = Inactive,
                         emphasis: Emphasis = NoEmphasis,
                         animated: Animated = NotAnimated,
                         icon: Option[Icon] = None,
                         size: Size = Size.NotSized,
                         buttonType: Type = ButtonType,
                         form: Option[String] = None,
                         basic: Boolean = false,
                         inverted: Boolean = false,
                         circular: Boolean = false,
                         labeled: Labeled = NotLabeled,
                         compact: Boolean = false,
                         disabled: Boolean = false,
                         loading: Boolean = false,
                         tabIndex: Option[Int] = None,
                         color: Option[String] = None,
                         onClickE: js.UndefOr[ReactEvent => Callback] = js.undefined,
                         onClick: Callback = Callback.empty,
                         dataTooltip: Option[String] = None,
                         extraStyles: List[Css] = Nil)

  private def classSet(p: Props): TagMod =
    ^.classSet(
      "active"        -> (p.state === Active),
      "primary"       -> (p.emphasis === Primary),
      "secondary"     -> (p.emphasis === Secondary),
      "animated"      -> (p.animated =!= NotAnimated),
      "vertical"      -> (p.animated === Vertical),
      "fade"          -> (p.animated === Fade),
      "icon"          -> p.icon.isDefined,
      "basic"         -> p.basic,
      "inverted"      -> p.inverted,
      "circular"      -> p.circular,
      "labeled"       -> (p.labeled === LeftLabeled),
      "right labeled" -> (p.labeled === RightLabeled),
      "disabled"      -> p.disabled,
      "loading"       -> p.loading,
      "compact"       -> p.compact,
      "tiny"          -> (p.size === Size.Tiny),
      "mini"          -> (p.size === Size.Mini),
      "small"         -> (p.size === Size.Small),
      "large"         -> (p.size === Size.Large),
      "big"           -> (p.size === Size.Big),
      "huge"          -> (p.size === Size.Huge),
      "massive"       -> (p.size === Size.Massive)
    )

  private def component =
    ScalaComponent
      .builder[Props]("Button")
      .renderPC(
        (_, p, c) =>
          if (p.animated === NotAnimated)
            <.button(
              ^.cls := "ui button",
              p.extraStyles,
              ^.`type` := (p.buttonType match {
                case ButtonType => "button"
                case SubmitType => "submit"
                case ResetType  => "reset"
              }),
              p.form.map(f => formId := f).whenDefined,
              p.color.map(u => ^.cls := u).whenDefined,
              p.dataTooltip.map(t => dataTooltip := t).whenDefined,
              classSet(p),
              p.onClickE.map { h =>
                ^.onClick ==> h
              }.getOrElse {
                ^.onClick --> p.onClick
              },
              p.icon.whenDefined,
              c
            )
          else {
            <.div(
              ^.cls := "ui button",
              ^.tabIndex :=? p.tabIndex,
              classSet(p),
              p.onClickE.map { h =>
                ^.onClick ==> h
              }.getOrElse {
                ^.onClick --> p.onClick
              },
              p.icon.whenDefined,
              c
            )
        })
      .build

  def apply(p: Props, children: VdomNode*): Unmounted[Props, Unit, Unit] =
    component(p)(children: _*)

  def apply(text: String): Unmounted[Props, Unit, Unit] =
    component(Props())(text)
}
