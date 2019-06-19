// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.table

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import seqexec.web.client.semanticui._
import japgolly.scalajs.react.component.Scala.Unmounted
import cats.implicits._

object TableHeader {
  final case class Props(collapsing: Boolean = false,
    colSpan: Option[Int] = None,
    aligned: Aligned = Aligned.None,
    width: Width = Width.None,
    key: String = ""
  )

  object Props {
    def zero: Props = Props()
  }

  private val component = ScalaComponent.builder[Props]("th")
    .stateless
    .renderPC((_, p, c) =>
      <.th(
        ^.classSet(
          "collapsing"     -> p.collapsing,
          "one wide"       -> (p.width === Width.One),
          "two wide"       -> (p.width === Width.Two),
          "three wide"     -> (p.width === Width.Three),
          "four wide"      -> (p.width === Width.Four),
          "five wide"      -> (p.width === Width.Five),
          "six wide"       -> (p.width === Width.Six),
          "seven wide"     -> (p.width === Width.Seven),
          "eight wide"     -> (p.width === Width.Eight),
          "nine wide"      -> (p.width === Width.Nine),
          "ten wide"       -> (p.width === Width.Ten),
          "eleven wide"    -> (p.width === Width.Eleven),
          "twelve wide"    -> (p.width === Width.Twelve),
          "thirteen wide"  -> (p.width === Width.Thirteen),
          "fourteen wide"  -> (p.width === Width.Fourteen),
          "fifteen wide"   -> (p.width === Width.Fifteen),
          "sixteen wide"   -> (p.width === Width.Sixteen),
          "left aligned"   -> (p.aligned === Aligned.Left),
          "center aligned" -> (p.aligned === Aligned.Center),
          "right aligned"  -> (p.aligned === Aligned.Right)
        ),
        p.colSpan.map(^.colSpan := _).whenDefined,
        c
      )
    ).build

  def apply(p: Props, children: VdomNode*): Unmounted[Props, Unit, Unit] = component(p)(children: _*)

  def apply(children: VdomNode*): Unmounted[Props, Unit, Unit] = component(Props.zero)(children: _*)
}
