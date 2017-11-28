// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.semanticui.elements.slider

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

import scalacss.ScalaCssReact._

object Slider {
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Props(label: String,
                   onClick    : Callback = Callback.empty,
                   extraStyles: List[scalacss.internal.StyleA] = Nil)

  private def component = ScalaComponent.builder[Props]("Slider")
    .renderPC((_, p, c) =>
      <.div(
        ^.cls := "ui slider checkbox",
        p.extraStyles.map(scalacssStyleaToTagMod).toTagMod,
        ^.onClick --> p.onClick,
        <.input(
          ^.`type` := "checkbox"),
        <.label(p.label),
        c
      )
    ).build

  def apply(p: Props, children: VdomNode*): Unmounted[Props, Unit, Unit] = component(p)(children: _*)

}
