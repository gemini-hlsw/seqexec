package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.model.{SectionOpen, SectionVisibilityState, WebSocketsLog}
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

import scalacss.ScalaCssReact._

object WebSocketsConsole {
  case class Props(searchArea: SectionVisibilityState, log: WebSocketsLog)

  private val component = ScalaComponent.builder[Props]("WebSocketsConsole")
    .stateless
    .render_P(p =>
      if (p.searchArea == SectionOpen) {
        <.div(
          ^.cls := "ui raised segments container",
          TextMenuSegment("WebSocket Console", "key.websockets.menu"),
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

  def apply(s: SectionVisibilityState, l:WebSocketsLog): Unmounted[Props, Unit, Unit] = component(Props(s, l))
}
