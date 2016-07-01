package edu.gemini.seqexec.web.client.components

import diode.ModelR
import diode.data.{Empty, Pot}
import diode.react.ReactPot._
import diode.react._
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconAttention, IconCheckmark, IconCircleNotched}
import edu.gemini.seqexec.web.client.semanticui.elements.message.CloseableMessage
import edu.gemini.seqexec.web.client.services.HtmlConstants.{iconEmpty, nbsp}
import edu.gemini.seqexec.web.common.{SeqexecQueue, Sequence, SequenceState}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import scalacss.ScalaCssReact._
import scalaz.syntax.show._

object QueueTableBody {
  case class Props(queue: ModelProxy[Pot[SeqexecQueue]], sectionOpen: SectionVisibilityState)

  // Minimum rows to display, pad with empty rows if needed
  val minRows = 5

  def emptyRow(k: String, sectionOpen: SectionVisibilityState) = {
    <.tr(
      ^.key := k, // React requires unique keys
      <.td(iconEmpty),
      <.td(
        sectionOpen == SectionOpen ?= SeqexecStyles.notInMobile,
        nbsp
      ),
      <.td(
        sectionOpen == SectionOpen ?= SeqexecStyles.notInMobile,
        nbsp
      ),
      <.td(
        sectionOpen == SectionOpen ?= SeqexecStyles.notInMobile,
        nbsp
      ),
      <.td(
        SeqexecStyles.notInMobile,
        nbsp)
    )
  }

  def load(p: Props):Callback =
    // Request to load the queue if not present
    Callback.when(p.queue.value.isEmpty)(p.queue.dispatch(UpdatedQueue(Empty)))

  def showSequence(p: Props,s: Sequence):Callback =
    // Request to display the selected sequence
    p.queue.dispatch(SelectToDisplay(s))

  val component = ReactComponentB[Props]("QueueTableBody")
    .render_P( p =>
      <.tbody(
        // Render after data arrives
        p.queue().render( q =>
          q.queue.map(Some.apply).padTo(minRows, None).zipWithIndex.collect {
            case (Some(s), i) =>
              <.tr(
                ^.classSet(
                  "positive" -> (s.state == SequenceState.Completed),
                  "warning"  -> (s.state == SequenceState.Running),
                  "negative" -> (s.state == SequenceState.Error),
                  "negative" -> (s.state == SequenceState.Abort)
                ),
                ^.key := s"item.queue.$i",
                ^.onClick --> showSequence(p, s),
                <.td(
                  ^.cls := "collapsing",
                  s.state match {
                    case SequenceState.Completed                   => IconCheckmark
                    case SequenceState.Running                     => IconCircleNotched.copy(IconCircleNotched.p.copy(loading = true))
                    case SequenceState.Error | SequenceState.Abort => IconAttention
                    case _                                         => iconEmpty
                  }
                ),
                <.td(
                  ^.cls := "collapsing",
                  p.sectionOpen == SectionOpen ?= SeqexecStyles.notInMobile,
                  s.id
                ),
                <.td(
                  p.sectionOpen == SectionOpen ?= SeqexecStyles.notInMobile,
                  s.state.shows + s.runningStep.map(u => s" ${u._1 + 1}/${u._2}").getOrElse("")
                ),
                <.td(
                  p.sectionOpen == SectionOpen ?= SeqexecStyles.notInMobile,
                  s.instrument
                ),
                <.td(
                  SeqexecStyles.notInMobile,
                  s.error.map(e => <.p(IconAttention, s" $e")).getOrElse(<.p("-"))
                )
              )
            case (_, i) =>
              emptyRow(s"item.queue.$i", p.sectionOpen)
          }
        ),
        // Render some rows when pending
        p.queue().renderPending(_ => (0 until minRows).map(i => emptyRow(s"item.queue.$i", p.sectionOpen))),
        // Render some rows even if it failed
        p.queue().renderFailed(_ => (0 until minRows).map(i => emptyRow(s"item.queue.$i", p.sectionOpen)))
      )
    )
    .componentDidMount($ => load($.props))
    .build

  def apply(p: ModelProxy[Pot[SeqexecQueue]], s: SectionVisibilityState) = component(Props(p, s))

}

/**
  * Shows a message when there is an error loading the queue
  */
object LoadingErrorMsg {
  case class Props(queue :ModelProxy[Pot[SeqexecQueue]])

  val component = ReactComponentB[Props]("LoadingErrorMessage")
    .stateless
    .render_P( p =>
      <.div(
        p.queue().renderFailed(_ =>
          CloseableMessage(CloseableMessage.Props(Some("Sorry, there was an error reading the queue from the server"), CloseableMessage.Style.Negative))
        )
      )
    )
    .build

  def apply(p: ModelProxy[Pot[SeqexecQueue]]) = component(Props(p))
}

object QueueTableLoading {
  case class Props(queue: Pot[SeqexecQueue])

  val component = ReactComponentB[Props]("QueueTableLoading")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui header item",
        p.queue.renderPending(_ => <.span(IconCircleNotched.copyIcon(loading = true), "Loading..."))
      )
    ).build

  def apply(p: ModelProxy[Pot[SeqexecQueue]]) = component(Props(p()))
}

/**
  * Component for the title of the queue area, including the search component
  */
object QueueAreaTitle {
  val statusAndSearchResultsConnect = SeqexecCircuit.connect(SeqexecCircuit.statusAndSearchResults)
  val queueConnect = SeqexecCircuit.connect(_.queue)

  case class Props(user: ModelProxy[Option[UserDetails]])

  val component = ReactComponentB[Props]("QueueAreaTitle")
    .stateless
    .render_P(p =>
      TextMenuSegment("Queue",
        // Show a loading indicator if we are waiting for server data
        {
          // Special equality check to avoid certain UI artifacts
          implicit val eq = PotEq.seqexecQueueEq
          queueConnect(QueueTableLoading.apply)
        },
        p.user().map { u =>
          <.div(
            ^.cls := "right menu",
            ^.key := "queue.area.title",
            statusAndSearchResultsConnect(SequenceSearch.apply)
          ): ReactNode
        }.getOrElse[ReactNode](<.div(^.key := "queue.area.empty"))
      )
    ).build

  def apply(user: ModelProxy[Option[UserDetails]]) = component(Props(user))
}

/**
  * Container for the queue table
  */
object QueueTableSection {
  val queueConnect = SeqexecCircuit.connect(_.queue)

  case class Props(opened: SectionVisibilityState)

  val component = ReactComponentB[Props]("QueueTableSection")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui segment scroll pane",
        SeqexecStyles.queueListPane,
        <.table(
          ^.cls := "ui selectable compact celled table unstackable",
          <.thead(
            <.tr(
              <.th(iconEmpty),
              <.th(
                p.opened == SectionOpen ?= SeqexecStyles.notInMobile,
                "Obs ID"
              ),
              <.th(
                p.opened == SectionOpen ?= SeqexecStyles.notInMobile,
                "State"
              ),
              <.th(
                p.opened == SectionOpen ?= SeqexecStyles.notInMobile,
                "Instrument"
              ),
              <.th(
                SeqexecStyles.notInMobile,
                "Notes"
              )
            )
          ),
          queueConnect(QueueTableBody(_, p.opened))
        )
      )
    ).build

  def apply(p: SectionVisibilityState) = component(Props(p))

}

/**
  * Displays the elements on the queue
  */
object QueueArea {
  val queueConnect = SeqexecCircuit.connect(_.queue)
  val userConnect = SeqexecCircuit.connect(_.user)

  case class Props(searchArea: ModelProxy[SectionVisibilityState])

  val component = ReactComponentB[Props]("QueueArea")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui raised segments container",
        userConnect(QueueAreaTitle(_)),
        <.div(
          ^.cls := "ui attached segment",
          <.div(
            ^.cls := "ui grid",
            <.div(
              ^.cls := "stretched row",
              <.div(
                ^.classSet(
                  "ten wide computer tablet one wide mobile column"     -> (p.searchArea() == SectionOpen),
                  "sixteen wide column"                                 -> (p.searchArea() == SectionClosed)
                ),
                // If there was an error on the process display a message
                queueConnect(LoadingErrorMsg(_)),
                QueueTableSection(p.searchArea())
              ),
              p.searchArea() == SectionOpen ?= SequenceSearchResults() // Display the search area if open
            )
          )
        )
      )
    )
    .build

  def apply(p: ModelProxy[SectionVisibilityState]) = component(Props(p))

}
