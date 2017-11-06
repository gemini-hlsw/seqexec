// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.model.GlobalLog
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

import scalaz.syntax.functor._

/**
  * Area to display a sequence's log
  */
object LogArea {
  final case class Props(log: GlobalLog)

  private val component = ScalaComponent.builder[Props]("LogArea")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui raised segments container",
        // TextMenuSegment("Log", "key.log.menu"),
        <.div(
          ^.cls := "ui sixteen wide column",
          <.div(
            ^.cls := "ui form",
            <.div(
              ^.cls := "field",
              <.textarea(
                ^.readOnly := true,
                ^.value := p.log.log.map(e => s"${e.timestamp} ${e.msg}").toVector.mkString("\n")
              )
            )
          )
        )
      )
    )
    .build

  def apply(p: ModelProxy[GlobalLog]): Unmounted[Props, Unit, Unit] = component(Props(p()))
}
