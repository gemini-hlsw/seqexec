package edu.gemini.seqexec.web.client.components


import diode.data.{Empty, Pot}
import diode.react.ReactPot._
import diode.react._
import edu.gemini.seqexec.web.client.model.UpdatedQueue
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.common.{SeqexecQueue, SequenceState}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import scalacss.ScalaCssReact._

case class Props(queue: ModelProxy[Pot[SeqexecQueue]])

object QueueTableBody {
  // Minimum rows to display, pad with empty rows if needed
  val minRows = 5

  def emptyRow(k: String) =
    <.tr(
      ^.key := k, // React requires unique keys
      <.td("\u00a0"),
      <.td("\u00a0"),
      <.td("\u00a0"),
      <.td(
        SeqexecStyles.notInMobile,
        "\u00a0")
    )

  val component = ReactComponentB[Props]("QueueTableBody")
    .stateless
    .render_P( p =>
      <.tbody(
        // Render after data arrives
        p.queue().render( q =>
          q.queue.map(Some.apply).padTo(minRows, None).collect {
            case Some(s) =>
              <.tr(
                ^.classSet(
                  "positive" -> (s.state == SequenceState.Running),
                  "negative" -> (s.state == SequenceState.Error)
                ),
                ^.key := s"item.queue.${s.id}",
                <.td(
                  ^.cls := "collapsing",
                  s.id
                ),
                <.td(s.state.toString),
                <.td(s.instrument),
                <.td(
                  SeqexecStyles.notInMobile,
                  s.error.map(_ => <.p(Icon("attention"), " Error")).getOrElse(<.p("-"))
                )
              )
            case _ =>
              emptyRow(s"time.queue." + (1000*math.random).toInt)
          }
        ),
        // Render some rows when pending
        p.queue().renderPending(_ => (0 until minRows).map(i => emptyRow(s"time.queue.$i")))
      )
    )
    .build

  def apply(p: ModelProxy[Pot[SeqexecQueue]]) = component(Props(p))

}

/**
  * Displays the elements on the queue
  */
object QueueArea {

  class Backend($: BackendScope[Props, Unit]) {
    def load(p: Props) =
      // Request to load the queue if not present
      Callback.ifTrue(p.queue.value.isEmpty, p.queue.dispatch(UpdatedQueue(Empty)))

    def render(p: Props) = {
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
                QueueTableBody(p.queue),
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
    .componentDidMount($ => $.backend.load($.props))
    .build

  def apply(p: ModelProxy[Pot[SeqexecQueue]]) = component(Props(p))

}
