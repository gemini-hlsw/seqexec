// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.icon

import cats.Eq
import cats.implicits._
import seqexec.web.client.semanticui._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ReactEvent
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.Reusability
import react.common.style._
import react.common.implicits._
import scala.scalajs.js

/**
  * Semantic UI Icon component
  */
final case class Icon(p: Icon.Props, children: Seq[VdomNode]) {
  import Icon._

  // Custom copy constructor to avoid passing the id again
  def copyIcon(disabled:     Boolean                            = false,
               loading:      Boolean                            = false,
               fitted:       Boolean                            = false,
               size:         Size                               = Size.NotSized,
               link:         Boolean                            = false,
               flipped:      Flipped                            = Flipped.NotFlipped,
               rotated:      Rotated                            = Rotated.NotRotated,
               circular:     Boolean                            = false,
               bordered:     Boolean                            = false,
               inverted:     Boolean                            = false,
               color:        Option[String]                     = None,
               extraStyles:  List[Css]                       = Nil,
               key:          String                             = "",
               onClickE:     js.UndefOr[ReactEvent => Callback] = js.undefined,
               onClick:      Callback                           = Callback.empty,
               onMouseEnter: Callback                           = Callback.empty,
               onMouseLeave: Callback                           = Callback.empty): Icon =
    copy(
      p = Icon.Props(
        id           = p.id,
        disabled     = disabled,
        loading      = loading,
        fitted       = fitted,
        size         = size,
        link         = link,
        flipped      = flipped,
        rotated      = rotated,
        circular     = circular,
        bordered     = bordered,
        inverted     = inverted,
        color        = color,
        extraStyles  = extraStyles,
        key          = key,
        onMouseEnter = onMouseEnter,
        onMouseLeave = onMouseLeave,
        onClickE     = onClickE,
        onClick      = onClick
      ),
      children = if (children.nonEmpty) children else this.children
    )

  private def component =
    ScalaComponent
      .builder[Props]("Icon")
      .stateless
      .renderPC((_, p, c) =>
        <.i(
          ^.cls := s"${p.id} icon",
          p.extraStyles,
          ^.cls :=? p.color,
          ^.classSet(
            "disabled" -> p.disabled,
            "loading" -> p.loading,
            "fitted" -> p.fitted,
            "tiny" -> (p.size === Size.Tiny),
            "mini" -> (p.size === Size.Mini),
            "small" -> (p.size === Size.Small),
            "large" -> (p.size === Size.Large),
            "big" -> (p.size === Size.Big),
            "huge" -> (p.size === Size.Huge),
            "massive" -> (p.size === Size.Massive),
            "link" -> p.link,
            "horizontally flipped" -> (p.flipped === Flipped.Horizontally),
            "vertically flipped" -> (p.flipped === Flipped.Vertically),
            "clockwise rotated" -> (p.rotated === Rotated.Clockwise),
            "counterclockwise rotated" -> (p.rotated === Rotated.CounterClockwise),
            "circular" -> p.circular,
            "bordered" -> p.bordered,
            "inverted" -> p.inverted
          ),
          p.onClickE
            .map { h =>
              ^.onClick ==> h
            }
            .getOrElse {
              ^.onClick --> p.onClick
            },
          ^.onMouseEnter --> p.onMouseEnter,
          ^.onMouseLeave --> p.onMouseLeave,
          c
      ))
      .configure(Reusability.shouldComponentUpdate)
      .build
      .withKey(p.key)
      .apply(p)(children: _*)
}

object Icon {
  implicit val iconProps: Reusability[Icon.Props] = Reusability
    .caseClassExcept[Icon.Props]('onClick,
                                 'onClickE,
                                 'onMouseEnter,
                                 'onMouseLeave)
  implicit val reuse: Reusability[Icon] = Reusability.by(_.p)
  val IconBrowser: Icon                 = Icon("browser")
  val IconDropdown: Icon                = Icon("dropdown")
  val IconInbox: Icon                   = Icon("inbox")
  val IconSettings: Icon                = Icon("settings")
  val IconBan: Icon                     = Icon("ban")
  val IconLock: Icon                    = Icon("lock")
  val IconRefresh: Icon                 = Icon("refresh")
  val IconReply: Icon                   = Icon("reply")
  val IconSignIn: Icon                  = Icon("sign in")
  val IconSignOut: Icon                 = Icon("sign out")
  val IconUpload: Icon                  = Icon("upload")
  val IconUser: Icon                    = Icon("user")
  val IconCircleNotched: Icon           = Icon("circle notched")
  val IconCrosshairs: Icon              = Icon("crosshairs")
  val IconCheckmark: Icon               = Icon("checkmark")
  val IconMinusCircle: Icon             = Icon("minus circle")
  val IconPlusSquareOutline: Icon       = Icon("plus square outline")
  val IconRemove: Icon                  = Icon("remove")
  val IconSelectedRadio: Icon           = Icon("dot circle outline")
  val IconAngleDoubleDown: Icon         = Icon("angle double down")
  val IconAngleDoubleUp: Icon           = Icon("angle double up")
  val IconCaretDown: Icon               = Icon("caret down")
  val IconCaretRight: Icon              = Icon("caret right")
  val IconChevronLeft: Icon             = Icon("chevron left")
  val IconChevronRight: Icon            = Icon("chevron right")
  val IconTrash: Icon                   = Icon("trash")
  val IconPause: Icon                   = Icon("pause")
  val IconPlay: Icon                    = Icon("play")
  val IconStop: Icon                    = Icon("stop")
  val IconStopCircle: Icon              = Icon("stop circle")
  val IconCopy: Icon                    = Icon("copy outline")
  val IconAttention: Icon               = Icon("attention")
  val IconClose: Icon                   = Icon("close")
  val IconCloneOutline: Icon            = Icon("clone outline")
  val IconTrashOutline: Icon            = Icon("trash alternate outline")
  val IconTimes: Icon                   = Icon("times")
  val IconSun: Icon                     = Icon("sun")
  val IconMoon: Icon                    = Icon("moon")
  val IconVolumeOff: Icon               = Icon("volume off")
  val IconVolumeUp: Icon                = Icon("volume up")
  val IconCircleOutline: Icon           = Icon("circle outline")
  val IconCheckCircleOutline: Icon      = Icon("check circle outline")
  val IconCalendarOutline: Icon         = Icon("calendar alternate outline")
  val IconClockOutline: Icon            = Icon("clock outline")

  sealed trait Flipped

  object Flipped {
    case object NotFlipped extends Flipped
    case object Horizontally extends Flipped
    case object Vertically extends Flipped

    implicit val equal: Eq[Flipped]          = Eq.fromUniversalEquals
    implicit val reuse: Reusability[Flipped] = Reusability.byRef[Flipped]
  }

  sealed trait Rotated

  object Rotated {
    case object NotRotated extends Rotated
    case object Clockwise extends Rotated
    case object CounterClockwise extends Rotated

    implicit val equal: Eq[Rotated]          = Eq.fromUniversalEquals
    implicit val reuse: Reusability[Rotated] = Reusability.byRef[Rotated]
  }

  final case class Props(id:           String,
                         disabled:     Boolean = false,
                         loading:      Boolean = false,
                         fitted:       Boolean = false,
                         size:         Size = Size.NotSized,
                         link:         Boolean = false,
                         flipped:      Flipped = Flipped.NotFlipped,
                         rotated:      Rotated = Rotated.NotRotated,
                         circular:     Boolean = false,
                         bordered:     Boolean = false,
                         inverted:     Boolean = false,
                         color:        Option[String] = None,
                         extraStyles:  List[Css] = Nil,
                         key:          String = "",
                         onMouseEnter: Callback = Callback.empty,
                         onMouseLeave: Callback = Callback.empty,
                         onClickE: js.UndefOr[ReactEvent => Callback] =
                           js.undefined,
                         onClick: Callback = Callback.empty)

  // Used to call Icon directly on a jsx component declaration
  implicit def icon2TagMod(i: Icon): VdomElement = i.component

  def apply(s: String, children: VdomNode*): Icon = Icon(Props(s), children)

}
