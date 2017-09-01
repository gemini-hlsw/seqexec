// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.semanticui.elements.divider

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted

/**
  * Semantic UI Divider component
  */
object Divider {
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Props(vertical: Boolean = false,
                   horizontal: Boolean = false,
                   inverted: Boolean = false,
                   fitted: Boolean = false,
                   hidden: Boolean = false,
                   section: Boolean = false,
                   clearing: Boolean = false)

  private def component = ScalaComponent.builder[Props]("Divider")
    .renderPC((_, p, c) =>
      <.div(
        ^.cls := "ui divider",
        ^.classSet(
          "vertical"   -> p.vertical,
          "horizontal" -> p.horizontal,
          "inverted"   -> p.inverted,
          "fitted"     -> p.fitted,
          "hidden"     -> p.hidden,
          "section"    -> p.section,
          "clearing"   -> p.clearing
        ),
        c
      )
    ).build

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(children: VdomNode*): Unmounted[Props, Unit, Unit] = component(Props())(children: _*)

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(p: Props, children: VdomNode*): Unmounted[Props, Unit, Unit] = component(p)(children: _*)
}
