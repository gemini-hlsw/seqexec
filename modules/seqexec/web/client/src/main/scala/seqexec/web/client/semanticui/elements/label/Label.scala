// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.label

import seqexec.web.client.semanticui._
import seqexec.web.client.semanticui.elements.icon.Icon
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import cats.implicits._
import react.common._
import react.common.syntax._
import cats.kernel.Eq

sealed trait Pointing

object Pointing {
  case object None extends Pointing
  case object Up extends Pointing
  case object Down extends Pointing
  case object Left extends Pointing
  case object Right extends Pointing

  implicit val equal: Eq[Pointing] = Eq.fromUniversalEquals
  implicit val reuse: Reusability[Pointing] = Reusability.byRef[Pointing]
}

object Label {
  final case class Props(text: String,
    htmlFor                  : Option[String] = None,
    color                    : Option[String] = None,
    tag                      : Boolean = false,
    basic                    : Boolean = false,
    link                     : Boolean = false,
    circular                 : Boolean = false,
    size                     : Size = Size.NotSized,
    pointing                 : Pointing = Pointing.None,
    icon                     : Option[Icon] = None,
    extraStyles              : List[Css] = Nil)

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  def content(p: Props): List[TagMod] = List(
    ^.cls := p.color.fold("ui label")(u => s"ui $u label"),
    p.extraStyles,
    ^.classSet(
      "basic"          -> p.basic,
      "tag"            -> p.tag,
      "circular"       -> p.circular,
      "tiny"           -> (p.size === Size.Tiny),
      "mini"           -> (p.size === Size.Mini),
      "small"          -> (p.size === Size.Small),
      "large"          -> (p.size === Size.Large),
      "big"            -> (p.size === Size.Big),
      "huge"           -> (p.size === Size.Huge),
      "massive"        -> (p.size === Size.Massive),
      "pointing"       -> (p.pointing === Pointing.Up),
      "pointing below" -> (p.pointing === Pointing.Down),
      "left pointing"  -> (p.pointing === Pointing.Left),
      "right pointing" -> (p.pointing === Pointing.Right)
    ),
    ^.htmlFor :=? p.htmlFor,
    p.icon.whenDefined,
    p.text
  )

  private val component = ScalaComponent.builder[Props]("Label")
    .stateless
    .renderPC((_, p, c) =>
      if (p.link) {
        <.a(
          content(p).toTagMod,
          c
        )
      } else {
        <.label(
          content(p).toTagMod,
          c
        )
      }
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props, children: VdomNode*): Unmounted[Props, Unit, Unit] = component(p)(children: _*)
}
