package edu.gemini.seqexec.web.client.components

import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

object WebSocketsConsole {

  val component = ReactComponentB[Unit]("WebSocketsConsole")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui raised segments container",
        TextMenuSegment("WebSocket Console"),
        <.div(
          ^.cls := "ui attached segment",
          <.div(
            ^.cls := "ui divided grid",
            <.div(
              ^.cls := "stretched row",
              <.div(
                ^.cls := "column sixteen wide",
                <.textarea(
                  ^.rows := 20,
                  ^.readOnly := true
                )
              )
            )
          )
        )
      )
    )
    .build

  def apply() = component()
}
