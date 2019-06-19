// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._

/**
  * Bar at the top of the page segtions
  */
final case class TextMenuSegment private (p: TextMenuSegment.Props, children: Seq[VdomNode], key: String) {
  import TextMenuSegment.Props

  private def component = ScalaComponent.builder[Props]("TextMenuSegment")
    .stateless
    .renderPC((_, p, c) =>
      <.div(
        ^.cls := "ui top attached text menu segment",
        ^.key := s"$key.text",
        <.div(
          ^.cls := "ui header item",
          ^.key := s"$key.item",
          p.header
        ),
        c
      )
    ).build.withKey(key).apply(p)(children: _*)
}

object TextMenuSegment {
  final case class Props(header: String)

  // Used to call TextMenuSegment directly on a jsx component declaration
  implicit def textMenu2TagMod(i: TextMenuSegment):VdomElement = i.component

  def apply(header: String, key: String, children: VdomNode*): TextMenuSegment = TextMenuSegment(Props(header), children, key)
}
