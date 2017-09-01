// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.semanticui.elements.label

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

object FormLabel {
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Props(text: String,
    htmlFor: Option[String] = None)

  private val component = ScalaComponent.builder[Props]("FormLabel")
    .stateless
    .renderPC((_, p, c) =>
      <.label(
        ^.htmlFor :=? p.htmlFor,
        p.text,
        c
      )
    ).build

  def apply(p: Props, children: VdomNode*): Unmounted[Props, Unit, Unit] = component(p)(children: _*)
}
