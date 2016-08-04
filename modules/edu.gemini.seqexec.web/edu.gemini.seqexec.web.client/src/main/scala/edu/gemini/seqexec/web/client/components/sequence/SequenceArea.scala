package edu.gemini.seqexec.web.client.components.sequence

import diode.react.{ModelProxy, ReactConnectProxy}
import diode.react.ReactPot._
import edu.gemini.seqexec.web.client.components.{SeqexecStyles, TabularMenu, TextMenuSegment}
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.divider.Divider
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconCaretRight, IconInbox, IconPause, IconPlay}
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconAttention, IconCheckmark, IconCircleNotched, IconStop}
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconChevronLeft
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import edu.gemini.seqexec.web.common.{Sequence, SequenceState, StepState}
import edu.gemini.seqexec.web.client.services.HtmlConstants.iconEmpty
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, Ref}

import scala.annotation.tailrec
import scalacss.ScalaCssReact._
import scalaz.syntax.show._
import org.scalajs.dom.raw.{Element, Node, HTMLElement}
import org.scalajs.dom.document

/**
  * Container for a table with the steps
  */
object SequenceStepsTableContainer {
  case class State(nextScrollPos: Double, autoScrolled: Boolean)

  case class Props(s: Sequence, status: ClientStatus, stepConfigDisplayed: Option[Int])

  def requestRun(s: Sequence): Callback = Callback {SeqexecCircuit.dispatch(RequestRun(s))}

  def requestPause(s: Sequence): Callback = Callback.log("Request pause")

  def requestStop(s: Sequence): Callback = Callback {SeqexecCircuit.dispatch(RequestStop(s))}

  def displayStepDetails(s: Sequence, i: Int): Callback = Callback {SeqexecCircuit.dispatch(ShowStep(s, i))}

  def backToSequence(s: Sequence): Callback = Callback {SeqexecCircuit.dispatch(UnShowStep(s))}

  // Reference to the specifc DOM marked by the name `scrollRef`
  val scrollRef = Ref[HTMLElement]("scrollRef")

  val component = ReactComponentB[Props]("HeadersSideBar")
    .initialState(State(0, autoScrolled = false))
    .renderPS { ($, p, s) =>
      <.div(
        ^.cls := "ui raised secondary segment",
        p.stepConfigDisplayed.fold {
          <.div(
            ^.cls := "row",
            p.status.isLogged && p.s.state == SequenceState.Abort ?=
              <.h3(
                ^.cls := "ui red header",
                "Sequence aborted"
              ),
            p.status.isLogged && p.s.state == SequenceState.Completed ?=
              <.h3(
                ^.cls := "ui green header",
                "Sequence completed"
              ),
            p.status.isLogged && p.s.state == SequenceState.NotRunning ?=
              Button(Button.Props(icon = Some(IconPlay), labeled = true, onClick = requestRun(p.s), disabled = !p.status.isConnected), "Run"),
            p.status.isLogged && p.s.state == SequenceState.Running ?=
              Button(Button.Props(icon = Some(IconPause), labeled = true, disabled = true, onClick = requestPause(p.s)), "Pause"),
            p.status.isLogged && p.s.state == SequenceState.Running ?=
              Button(Button.Props(icon = Some(IconStop), labeled = true, onClick = requestStop(p.s), disabled = !p.status.isConnected), "Stop")
          )
        } { i =>
          <.div(
            ^.cls := "row",
            Button(Button.Props(icon = Some(IconChevronLeft), onClick = backToSequence(p.s)), "Back"),
            <.h5(
              ^.cls := "ui header",
              SeqexecStyles.inline,
              s" Configuration for step ${i + 1}"
            )
          )
        },
        Divider(),
        <.div(
          ^.cls := "ui row scroll pane",
          SeqexecStyles.stepsListPane,
          ^.ref := scrollRef,
          p.stepConfigDisplayed.map { i =>
            val step = p.s.steps.steps.find(_.id == i)
            <.table(
              ^.cls := "ui selectable compact celled table unstackable",
              <.thead(
                <.tr(
                  <.th(
                    ^.cls := "collapsing",
                    "Name"
                  ),
                  <.th(
                    ^.cls := "six wide",
                    "Value"
                  )
                )
              ),
              <.tbody(
                step.map(_.config).getOrElse(Nil).map(c =>
                  <.tr(
                    ^.classSet(
                      "positive" -> c.key.startsWith("instrument"),
                      "warning"  -> c.key.startsWith("telescope")
                    ),
                    c.key.startsWith("observe") ?= SeqexecStyles.observeConfig,
                    c.key.startsWith("ocs") ?= SeqexecStyles.observeConfig,
                    <.td(
                      c.key
                    ),
                    <.td(
                      c.value
                    )
                  )
                )
              )
            )
          }.getOrElse {
            <.table(
              ^.cls := "ui selectable compact celled table unstackable",
              <.thead(
                <.tr(
                  <.th(
                    ^.cls := "collapsing",
                    iconEmpty
                  ),
                  <.th(
                    ^.cls := "collapsing",
                    "Step"
                  ),
                  <.th(
                    ^.cls := "six wide",
                    "State"
                  ),
                  <.th(
                    ^.cls := "ten wide",
                    "File"
                  ),
                  <.th(
                    ^.cls := "collapsing",
                    "Config"
                  )
                )
              ),
              <.tbody(
                SeqexecStyles.stepsListBody,
                p.s.steps.steps.map(s =>
                  <.tr(
                    ^.classSet(
                      "positive" -> (s.state == StepState.Done),
                      "warning"  -> (s.state == StepState.Running),
                      "negative" -> (s.state == StepState.Error),
                      "negative" -> (s.state == StepState.Abort)
                    ),
                    s.state == StepState.Running ?= SeqexecStyles.stepRunning,
                    <.td(
                      s.state match {
                        case StepState.Done    => IconCheckmark
                        case StepState.Running => IconCircleNotched.copyIcon(loading = true)
                        case StepState.Error   => IconAttention
                        case _                 => iconEmpty
                      }
                    ),
                    <.td(s.id + 1),
                    <.td(s.state.shows),
                    <.td(s.file.getOrElse(""): String),
                    <.td(
                      ^.cls := "collapsing right aligned",
                      IconCaretRight.copyIcon(onClick = displayStepDetails(p.s, s.id))
                    )
                  )
                )
              )
            )
          }
        )
      )
    }
    .componentWillReceiveProps { f =>
      // Called when the props have changed. At this time we can recalculate
      // if the scroll position needs to be updated and store it in the State
      val div = scrollRef(f.$)
      if (f.nextProps.s.id != f.currentProps.s.id) {
        // It will reset to 0 if the sequence changes
        // TODO It may be better to remember the pos of executed steps per sequence
        f.$.setState(State(0, autoScrolled = true))
      } else {
        div.fold(Callback.empty) { scrollPane =>
          /**
            * Calculates if the element is visible inside the scroll pane up the dom tree
            */
          def visibleY(el: Element): Boolean = {
            val rect = el.getBoundingClientRect()
            val top = rect.top
            val height = rect.height

            @tailrec
            def go(el: Node): Boolean =
              el match {
                case e: Element if e.classList.contains(SeqexecStyles.stepsListPane.htmlClass) =>
                  (top + height) <= (e.getBoundingClientRect().top + e.getBoundingClientRect().height)
                // Fallback to the document in nothing else
                case e if el.parentNode == document.body                                       =>
                  top <= document.documentElement.clientHeight
                case e: Element                                                                =>
                  go(el.parentNode)
              }

            go(el.parentNode)
          }

          /**
            * Calculates the new scroll position if the relevant row is not visible
            */
          def scrollPosition: Option[Double] = {
            val progress = f.nextProps.s.steps
            // Build a css selector for the relevant row, either the last one when complete
            // or the currently running one
            val rowSelector = if (progress.allStepsDone) {
              s".${SeqexecStyles.stepsListBody.htmlClass} tr:last-child"
            } else {
              s".${SeqexecStyles.stepsListBody.htmlClass} tr.${SeqexecStyles.stepRunning.htmlClass}"
            }
            Option(scrollPane.querySelector(rowSelector)).map(n => (n, n.parentNode)).collect {
              case (e: HTMLElement, parent: HTMLElement) if !visibleY(e) =>
                e.offsetTop - parent.offsetTop
              }
          }

          // If the scroll position is defined update the state
          scrollPosition.fold(Callback.empty) { p =>
            f.$.setState(State(p, autoScrolled = true))
          }
        }
      }
    }.componentWillUpdate { f =>
      // Called before the DOM is rendered on the updated props. This is the chance
      // to update the scroll position if needed
      val div = scrollRef(f.$)
      div.fold(Callback.empty){ scrollPane =>
        // If the state indicates to scroll, update the scroll position
        Callback.when(f.nextState.autoScrolled)(Callback {
            scrollPane.scrollTop = f.nextState.nextScrollPos
          }
        )
      }
    }.build

  def apply(s: Sequence, status: ClientStatus, stepConfigDisplayed: Option[Int]) = component(Props(s, status, stepConfigDisplayed))
}

/**
  * Content of a single tab with a sequence
  */
object SequenceTabContent {

  case class Props(isActive: Boolean, status: ClientStatus, st: SequenceTab)

  val component = ReactComponentB[Props]("SequenceTabContent")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui bottom attached tab segment",
        ^.classSet(
          "active" -> p.isActive
        ),
        dataTab := p.st.instrument,
        p.st.sequence().render { s =>
          SequenceStepsTableContainer(s, p.status, p.st.stepConfigDisplayed)
        },
        p.st.sequence().renderEmpty(IconMessage(IconMessage.Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning)))
      )
    )
    .build

  def apply(p: Props) = component(p)
}

/**
  * Contains the area with tabs and the sequence body
  */
object SequenceTabsBody {
  case class Props(s: ClientStatus, d: SequencesOnDisplay)
  def tabContents(status: ClientStatus, d: SequencesOnDisplay): Stream[SequenceTabContent.Props] = d.instrumentSequences.map(a => SequenceTabContent.Props(isActive = a == d.instrumentSequences.focus, status, a)).toStream

  val component = ReactComponentB[Props]("SequenceTabsBody")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "twelve wide computer twelve wide tablet sixteen wide mobile column",
        TabularMenu(p.d),
        tabContents(p.s, p.d).map(SequenceTabContent.apply)
      )
    )
    .build

  def apply(p: ModelProxy[(ClientStatus, SequencesOnDisplay)]) = component(Props(p()._1, p()._2))
}

/**
  * Contains all the tabs for the sequences available in parallel
  * All connects at this level, be careful about adding connects below here
  */
object SequenceTabs {
  val logConnect: ReactConnectProxy[GlobalLog] = SeqexecCircuit.connect(_.globalLog)
  val sequencesDisplayConnect: ReactConnectProxy[(ClientStatus, SequencesOnDisplay)] = SeqexecCircuit.connect(SeqexecCircuit.statusAndSequences)

  case class Props(status: ClientStatus, sequences: SequencesOnDisplay)

  val component = ReactComponentB[Unit]("SequenceTabs")
    .stateless
    .render( _ =>
      <.div(
        ^.cls := "ui bottom attached segment",
        <.div(
          ^.cls := "ui two column vertically divided grid",
          <.div(
            ^.cls := "row",
            <.div(
              ^.cls := "four wide column computer tablet only",
              HeadersSideBar()
            ),
            sequencesDisplayConnect(SequenceTabsBody.apply)
          ),
          <.div(
            ^.cls := "row computer only",
            <.div(
              ^.cls := "sixteen wide column",
              logConnect(LogArea.apply)
            )
          )
        )
      )
    )
    .build

  def apply() = component()
}

object SequenceArea {

  val component = ReactComponentB[Unit]("QueueTableSection")
    .stateless
    .render( _ =>
      <.div(
        ^.cls := "ui raised segments container",
        TextMenuSegment("Running Sequences", "key.sequences.menu"),
        SequenceTabs()
      )
    ).build

  def apply() = component()
}
