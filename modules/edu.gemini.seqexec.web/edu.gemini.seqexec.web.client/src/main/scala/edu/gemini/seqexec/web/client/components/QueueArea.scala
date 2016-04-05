package edu.gemini.seqexec.web.client.components

import diode.data.{Empty, Pot}
import diode.react.ReactPot._
import diode.react._
import edu.gemini.seqexec.web.client.model.UpdatedQueue
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.client.semanticui.elements.message.CloseableMessage
import edu.gemini.seqexec.web.common.{SeqexecQueue, Sequence, SequenceState}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import scalacss.ScalaCssReact._

object QueueTableBody {
  case class Props(queue: ModelProxy[Pot[SeqexecQueue]])

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
          q.queue.map(Some.apply).padTo(minRows, None).zipWithIndex.collect {
            case (Some(s), i) =>
              <.tr(
                ^.classSet(
                  "positive" -> (s.state == SequenceState.Running),
                  "negative" -> (s.state == SequenceState.Error)
                ),
                ^.key := s"item.queue.$i",
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
            case (_, i) =>
              emptyRow(s"item.queue.$i")
          }
        ),
        // Render some rows when pending
        p.queue().renderPending(_ => (0 until minRows).map(i => emptyRow(s"item.queue.$i"))),
        // Render some rows even if it failed
        p.queue().renderFailed(_ => (0 until minRows).map(i => emptyRow(s"item.queue.$i")))
      )
    )
    .build

  def apply(p: ModelProxy[Pot[SeqexecQueue]]) = component(Props(p))

}

/**
  * Displays the elements on the queue
  */
object QueueArea {
  case class Props(queue: ModelProxy[Pot[SeqexecQueue]], searchResults: ModelProxy[Pot[List[Sequence]]])

  def load(p: Props) =
    // Request to load the queue if not present
    Callback.ifTrue(p.queue.value.isEmpty, p.queue.dispatch(UpdatedQueue(Empty)))

  val component = ReactComponentB[Props]("QueueArea")
    .stateless
    .render_P(p =>
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
                SequenceSearch(p.searchResults)
              )
            ),
            <.div(
              ^.cls := "ui secondary segment",
              // Show a loading indicator if we are waiting for server data
              p.queue().renderPending(_ => <.div(
                ^.cls := "ui active dimmer",
                <.div(
                  ^.cls := "ui text loader large",
                  "Loading")
              )),
              // If there was an error on the process display a message
              p.queue().renderFailed(_ =>
                CloseableMessage(CloseableMessage.Props(Some("Sorry, there was an error reading the queue from the server"), CloseableMessage.Style.Negative))
              ),
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
    )
    .componentDidMount($ => load($.props))
    .build

  def apply(p: ModelProxy[Pot[SeqexecQueue]], s: ModelProxy[Pot[List[Sequence]]]) = component(Props(p, s))

}
