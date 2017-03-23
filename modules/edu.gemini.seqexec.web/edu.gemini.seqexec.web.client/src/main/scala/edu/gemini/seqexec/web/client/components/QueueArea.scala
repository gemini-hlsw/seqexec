package edu.gemini.seqexec.web.client.components

import diode.react._
import edu.gemini.seqexec.model.Model.{SequenceState, SequenceView}
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.model.ModelOps._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconAttention, IconCheckmark, IconCircleNotched}
import edu.gemini.seqexec.web.client.services.HtmlConstants.{iconEmpty, nbsp}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._
import org.scalajs.dom.html.TableRow

import scala.scalajs.js
import scalacss.ScalaCssReact._
import scalaz.syntax.show._

object QueueTableBody {
  case class Props(sequences: ModelProxy[SeqexecAppRootModel.LoadedSequences], sectionOpen: SectionVisibilityState)

  // Minimum rows to display, pad with empty rows if needed
  val minRows = 5

  def emptyRow(k: String, sectionOpen: SectionVisibilityState): ReactTagOf[TableRow] = {
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

  def showSequence(p: Props,s: SequenceView): Callback =
    // Request to display the selected sequence
    p.sequences.dispatchCB(SelectToDisplay(s))

  private val component = ReactComponentB[Props]("QueueTableBody")
    .render_P( p =>
      <.tbody(
        // Render after data arrives
        p.sequences().queue.map(Some.apply).padTo(minRows, None).zipWithIndex.collect {
            case (Some(s), i) =>
              <.tr(
                ^.classSet(
                  "positive" -> (s.status == SequenceState.Completed),
                  "warning"  -> (s.status == SequenceState.Running),
                  "negative" -> s.hasError
                  //"negative" -> (s.status == SequenceState.Abort)
                ),
                ^.key := s"item.queue.$i",
                ^.onClick --> showSequence(p, s),
                <.td(
                  ^.cls := "collapsing",
                  s.status match {
                    case SequenceState.Completed                   => IconCheckmark
                    case SequenceState.Running                     => IconCircleNotched.copy(IconCircleNotched.p.copy(loading = true))
                    case SequenceState.Error(_)                    => IconAttention
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
                  s.status.shows + s.runningStep.map(u => s" ${u._1 + 1}/${u._2}").getOrElse("")
                ),
                <.td(
                  p.sectionOpen == SectionOpen ?= SeqexecStyles.notInMobile,
                  s.metadata.instrument
                ),
                <.td(
                  SeqexecStyles.notInMobile//,
                  //s.error.map(e => <.p(IconAttention, s" $e")).getOrElse(<.p("-"))
                )
              )
            case (_, i) =>
              emptyRow(s"item.queue.$i", p.sectionOpen)
          }
      )
    )
    .build

  def apply(p: ModelProxy[SeqexecAppRootModel.LoadedSequences], s: SectionVisibilityState): ReactComponentU[Props, Unit, Unit, TopNode] = component(Props(p, s))

}

/**
  * Shows a message when there is an error loading the queue
  */
object LoadingErrorMsg {
  case class Props(queue :ModelProxy[SeqexecAppRootModel.LoadedSequences])

  private val component = ReactComponentB[Props]("LoadingErrorMessage")
    .stateless
    .render_P( p =>
      <.div(
        /*p.queue().renderFailed(_ =>
          CloseableMessage(CloseableMessage.Props(Some("Sorry, there was an error reading the queue from the server"), CloseableMessage.Style.Negative))
        )*/
      )
    )
    .build

  def apply(p: ModelProxy[SeqexecAppRootModel.LoadedSequences]): ReactComponentU[Props, Unit, Unit, TopNode] = component(Props(p))
}

/**
  * Component for the title of the queue area, including the search component
  */
object QueueAreaTitle {
  private val statusAndSearchResultsConnect = SeqexecCircuit.connect(SeqexecCircuit.statusAndSearchResults, "key.queue.search": js.Any)
  private val queueConnect = SeqexecCircuit.connect(_.sequences, "key.queue.area": js.Any)

  case class Props(user: ModelProxy[Option[UserDetails]])

  private val component = ReactComponentB[Props]("QueueAreaTitle")
    .stateless
    .render_P(p =>
      TextMenuSegment("Night Queue", "key.queue.menu",
        p.user().fold(<.div()) { _ =>
          <.div(
            ^.cls := "right menu",
            statusAndSearchResultsConnect(SequenceSearch.apply)
          )
        }
      )
    ).build.withKey("key.area.title")

  def apply(user: ModelProxy[Option[UserDetails]]): ReactComponentU[Props, Unit, Unit, TopNode] = component(Props(user))
}

/**
  * Container for the queue table
  */
object QueueTableSection {
  private val queueConnect = SeqexecCircuit.connect(_.sequences, "key.queue": js.Any)

  case class Props(opened: SectionVisibilityState)

  private val component = ReactComponentB[Props]("QueueTableSection")
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

  def apply(p: SectionVisibilityState): ReactComponentU[Props, Unit, Unit, TopNode] = component(Props(p))

}

/**
  * Displays the elements on the queue
  */
object QueueArea {
  private val sequencesConnect = SeqexecCircuit.connect(_.sequences)
  private val userConnect = SeqexecCircuit.connect(_.user)

  case class Props(searchArea: ModelProxy[SectionVisibilityState])

  private val component = ReactComponentB[Props]("QueueArea")
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
                  "ten wide computer two wide tablet one wide mobile column" -> (p.searchArea() == SectionOpen),
                  "sixteen wide column"                                      -> (p.searchArea() == SectionClosed)
                ),
                // If there was an error on the process display a message
                sequencesConnect(LoadingErrorMsg(_)),
                QueueTableSection(p.searchArea())
              ),
              p.searchArea() == SectionOpen ?= SequenceLoad() // Display the search area if open
            )
          )
        )
      )
    )
    .build

  def apply(p: ModelProxy[SectionVisibilityState]): ReactComponentU[Props, Unit, Unit, TopNode] = component(Props(p))

}
