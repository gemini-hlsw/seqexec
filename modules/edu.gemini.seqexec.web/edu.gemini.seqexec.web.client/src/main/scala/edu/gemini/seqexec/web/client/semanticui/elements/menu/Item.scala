package edu.gemini.seqexec.web.client.semanticui.elements.menu

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

/**
  * Semantic UI Menu Item component
  */
object Item {
  case class Props(name: String)

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
