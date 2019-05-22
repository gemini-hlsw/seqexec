// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.label

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

object FormLabel {
  final case class Props(text: String, htmlFor: Option[String])
  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val component = ScalaComponent.builder[Props]("FormLabel")
    .stateless
    .renderPC((_, p, c) =>
      <.label(
        ^.htmlFor :=? p.htmlFor,
        p.text,
        c
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props, children: VdomNode*): Unmounted[Props, Unit, Unit] = component(p)(children: _*)
}
