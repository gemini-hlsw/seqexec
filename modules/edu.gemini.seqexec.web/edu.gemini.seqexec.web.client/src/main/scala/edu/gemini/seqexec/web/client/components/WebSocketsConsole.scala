package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.model.{SectionOpen, SectionVisibilityState, WebSocketsLog}
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._

object WebSocketsConsole {
  case class Props(searchArea: SectionVisibilityState, log: WebSocketsLog)

  val component = ReactComponentB[Props]("WebSocketsConsole")
    .stateless
    .render_P(p =>
      if (p.searchArea == SectionOpen) {
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
                    SeqexecStyles.smallTextArea,
                    ^.rows := 20,
                    ^.readOnly := true,
                    ^.value := p.log.log.mkString("\n")
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

  def apply(s: SectionVisibilityState, l:WebSocketsLog) = component(Props(s, l))
}
