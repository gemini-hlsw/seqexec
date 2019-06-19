// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.button

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import react.common.style._
import react.common.implicits._

object ButtonGroup {

  final case class Props(extraStyles: List[Css] = Nil)

  private def component =
    ScalaComponent
      .builder[Props]("ButtonGroup")
      .renderPC(
        (_, p, c) =>
          <.div(
            p.extraStyles,
            ^.cls := "ui buttons",
            c
        ))
      .build

  def apply(p: Props, children: VdomNode*): Unmounted[Props, Unit, Unit] =
    component(p)(children: _*)

  def apply(children: VdomNode*): Unmounted[Props, Unit, Unit] =
    component(Props())(children: _*)
}
