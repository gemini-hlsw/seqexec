package edu.gemini.seqexec.web.client.components.sequence

import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

/**
  * Area to display a sequence's log
  */
object LogArea {
  val component = ReactComponentB[Unit]("LogArea")
    .stateless
    .render(_ =>
      <.div(
        ^.cls := "ui raised secondary segment",
        <.h4("Log"),
        <.div(
          ^.cls := "ui form",
          <.div(
            ^.cls := "field",
            <.textarea()
          )
        )
      )
    )
    .build

  def apply() = component()
}
