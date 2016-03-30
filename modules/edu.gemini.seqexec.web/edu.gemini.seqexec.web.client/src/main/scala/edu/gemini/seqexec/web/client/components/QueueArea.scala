package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.common.SeqexecQueue
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import scalacss.ScalaCssReact._

/**
  * Displays the elements on the queue
  */
object QueueArea {

  case class Props(queue: ModelProxy[Option[SeqexecQueue]])

  class Backend($: BackendScope[Props, Unit]) {
    def render() = {
      <.div(
        ^.cls := "ui grid container",
        <.div(
          ^.cls := "sixteen wide column",
          <.div(
            ^.cls := "ui raised segments",
            <.div(
              ^.cls := "ui top attached text menu",
              <.div(
                ^.cls := "ui header item",
                "Queue"
              ),
              <.div(
                ^.cls := "right menu",
                <.div(
                  ^.cls := "ui right aligned category search item",
                  <.div(
                    ^.cls := "ui transparent icon input",
                    <.input(
                      ^.cls := "prompt",
                      ^.`type` := "text",
                      ^.placeholder := "Search..."
                    ),
                    Icon("search link")
                  ),
                  <.div(
                    ^.cls := "results"
                  )
                )
              )
            ),
            <.div(
              ^.cls := "ui secondary segment",
              <.table(
                ^.cls := "ui selectable compact celled table unstackable",
                <.thead(
                  <.tr(
                    <.th("Obs ID"),
                    <.th("State"),
                    <.th("Instrument"),
                    <.th(
                      SeqexecStyles.notInMobile,
                      "Notes"
                    )
                  )
                ),
                <.tbody(
                  <.tr(
                    <.td(
                      ^.cls := "collapsing",
                      "GS-2016A-Q-0-1"
                    ),
                    <.td("Not Started"),
                    <.td("GPI"),
                    <.td(
                      SeqexecStyles.notInMobile,
                      "-"
                    )
                  ),
                  <.tr(
                    ^.cls := "positive",
                    <.td("GS-2016A-Q-5-3"),
                    <.td("Running"),
                    <.td("GMOS-S"),
                    <.td(
                      SeqexecStyles.notInMobile,
                      "-"
                    )
                  ),
                  <.tr(
                    ^.cls := "negative",
                    <.td("GS-2016A-Q-4-1"),
                    <.td("Error"),
                    <.td("Flamingos 2"),
                    <.td(
                      SeqexecStyles.notInMobile,
                      Icon("attention"),
                      " Error"
                    )
                  ),
                  <.tr(
                    <.td("\u00a0"),
                    <.td(" "),
                    <.td(" "),
                    <.td(
                      SeqexecStyles.notInMobile,
                      " ")
                  ),
                  <.tr(
                    <.td("\u00a0"),
                    <.td(" "),
                    <.td(" "),
                    <.td(
                      SeqexecStyles.notInMobile,
                      " ")
                  )
                ),
                <.tfoot(
                  <.tr(
                    <.th(
                      ^.colSpan := "4",
                      <.div(
                        ^.cls := "ui right floated pagination menu",
                        <.a(
                          ^.cls := "icon item",
                          Icon("left chevron")
                        ),
                        <.a(
                          ^.cls := "item","1"),
                        <.a(
                          ^.cls := "item","2"),
                        <.a(
                          ^.cls := "item","3"),
                        <.a(
                          ^.cls := "item","4"),
                        <.a(
                          ^.cls := "icon item",
                          Icon("right chevron")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
  }

  val component = ReactComponentB[Props]("QueueArea")
    .stateless
    .renderBackend[Backend]
    .build

  def apply(p: ModelProxy[Option[SeqexecQueue]]) = component(Props(p))

}
