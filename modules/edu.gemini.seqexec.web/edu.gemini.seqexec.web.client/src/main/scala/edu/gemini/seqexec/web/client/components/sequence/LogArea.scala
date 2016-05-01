package edu.gemini.seqexec.web.client.components.sequence

import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.model.GlobalLog
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

/**
  * Area to display a sequence's log
  */
object LogArea {
  case class Props(log: GlobalLog)

  val component = ReactComponentB[Props]("LogArea")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui raised secondary segment",
        <.h4("Log"),
        <.div(
          ^.cls := "ui form",
          <.div(
            ^.cls := "field",
            <.textarea(
              ^.readOnly := true,
              ^.value := p.log.log.map(e => s"${e.timestamp} ${e.s}").mkString("\n")
            )
          )
        )
      )
    )
    .build

  def apply(p: ModelProxy[GlobalLog]) = component(Props(p()))
}
