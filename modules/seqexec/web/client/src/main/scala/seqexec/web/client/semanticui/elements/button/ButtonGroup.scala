// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.button

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import web.client.style._

object ButtonGroup {

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Props(extraStyles: List[GStyle] = Nil)

  private def component =
    ScalaComponent
      .builder[Props]("ButtonGroup")
      .renderPC(
        (_, p, c) =>
          <.div(
            p.extraStyles.map(geminiStyleToTagMod).toTagMod,
            ^.cls := "ui buttons",
            c
        ))
      .build

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(p: Props, children: VdomNode*): Unmounted[Props, Unit, Unit] =
    component(p)(children: _*)

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(children: VdomNode*): Unmounted[Props, Unit, Unit] =
    component(Props())(children: _*)
}
