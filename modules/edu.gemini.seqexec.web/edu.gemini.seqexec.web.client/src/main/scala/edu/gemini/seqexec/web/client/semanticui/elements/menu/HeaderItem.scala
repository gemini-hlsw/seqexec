// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.semanticui.elements.menu

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

/**
  * Semantic UI Header Item component
  */
object HeaderItem {

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Props(name: String, sub: Boolean = false, extraStyles: List[scalacss.internal.StyleA] = Nil)

  private val component = ScalaComponent.builder[Props]("Header-Item")
    .stateless
    .renderPC( (_, p, c) =>
      <.div(
        ^.cls := "ui header item",
        p.extraStyles.map(scalacssStyleaToTagMod).toTagMod,
        ^.classSet(
          "sub" -> p.sub
        ),
        p.name,
        c
      )
    ).build

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(name: String, children: VdomNode*): Unmounted[Props, Unit, Unit] = component(Props(name))(children: _*)

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(p: Props, children: VdomNode*): Unmounted[Props, Unit, Unit] = component(p)(children: _*)
}
