package edu.gemini.seqexec.web.client.semanticui.elements.label

import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

object Label {
  case class Props(text: String,
    htmlFor: Option[String] = None,
    color  : Option[String] = None,
    tag    : Boolean = false,
    basic  : Boolean = false,
    icon   : Option[Icon] = None)

  private val component = ScalaComponent.builder[Props]("Label")
    .stateless
    .renderPC((_, p, c) =>
      <.label(
        ^.cls := "ui label",
        ^.classSet(
          "basic" -> p.basic,
          "tag"   -> p.tag
        ),
        p.color.map(u => ^.cls := u).whenDefined,
        ^.htmlFor :=? p.htmlFor,
        p.icon.whenDefined,
        p.text,
        c
      )
    ).build

  def apply(p: Props, children: VdomNode*): Unmounted[Props, Unit, Unit] = component(p)(children: _*)
}
