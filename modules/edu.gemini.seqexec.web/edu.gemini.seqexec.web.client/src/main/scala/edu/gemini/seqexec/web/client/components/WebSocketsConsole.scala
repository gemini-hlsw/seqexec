package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.model.{SectionOpen, SectionVisibilityState}
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

object WebSocketsConsole {
  case class Props(searchArea: ModelProxy[SectionVisibilityState])

  val component = ReactComponentB[Props]("WebSocketsConsole")
    .stateless
    .render_P(p =>
      if (p.searchArea() == SectionOpen) {
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
      } else {
        <.div()
      }
    )
    .build

  def apply(p: ModelProxy[SectionVisibilityState]) = component(Props(p))
}
