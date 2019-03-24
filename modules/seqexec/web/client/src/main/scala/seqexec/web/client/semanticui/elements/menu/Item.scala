// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.menu

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

/**
  * Semantic UI Menu Item component
  */
object Item {
  final case class Props(name: String)

  private val component = ScalaComponent.builder[Props]("Item")
    .stateless
    .renderPC( (_, p, c) =>
      <.div(
        ^.cls := "item",
        p.name,
        c
      )
    ).build

  def apply(name: String, children: VdomNode*): Unmounted[Props, Unit, Unit] = component(Props(name))(children: _*)
}
