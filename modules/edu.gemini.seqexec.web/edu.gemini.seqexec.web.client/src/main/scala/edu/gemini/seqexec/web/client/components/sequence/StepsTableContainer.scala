package edu.gemini.seqexec.web.client.components.sequence

import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactNode, Ref}
import japgolly.scalajs.react.vdom.prefix_<^._

import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.table.TableHeader
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.model.Model.ObservationOperations._
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.model.ModelOps._
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.services.HtmlConstants.iconEmpty


import scalacss.ScalaCssReact._
import scalaz.syntax.show._
import scalaz.syntax.equal._
import scalaz.syntax.std.boolean._

import org.scalajs.dom.raw.{Element, HTMLElement, Node}
import org.scalajs.dom.document
import org.scalajs.dom.html.Div

import scala.annotation.tailrec

/**
  * Container for a table with the steps
  */
object StepsTableContainer {
  case class State(nextScrollPos  : Double,
                   onHover        : Option[Int],
                   autoScrolled   : Boolean)

  case class Props(s: SequenceView, status: ClientStatus, stepConfigDisplayed: Option[Int], nextStepToRun: Int, onStepToRun: Int => Callback)

  class Backend($: BackendScope[Props, State]) {

    def configTable(step: Step): TagMod =
      <.table(
        ^.cls := "ui selectable compact celled table unstackable",
        <.thead(
          <.tr(
            TableHeader(TableHeader.Props(collapsing = true), "Name"),
            TableHeader(TableHeader.Props(width = Width.Six), "Value")
          )
        ),
        <.tbody(
          step.config.map {
            case (sub, c) =>
              c.map {
                case (k, v) =>
                  <.tr(
                    ^.classSet(
                      "positive" -> sub.startsWith("instrument"),
                      "warning"  -> sub.startsWith("telescope")
                    ),
                    k.startsWith("observe") ?= SeqexecStyles.observeConfig,
                    k.startsWith("ocs") ?= SeqexecStyles.observeConfig,
                    <.td(k),
                    <.td(v)
                  )
              }
            }
        )
      )

    def stepProgress(step: Step): ReactNode =
      step.status match {
        case StepState.Running =>
          <.div(
            ^.cls := "ui progress vcentered",
            <.div(
              ^.cls := "bar",
              <.div(
                ^.cls := "progress")
            )
          )
        case StepState.Completed =>
          step.fileId.getOrElse(""): String
        case _ =>
          step.file.getOrElse(""): String
      }

    def observationControlButtons(s: SequenceView, step: Step): List[ReactNode] = {
      s.allowedObservationOperations(step).map {
        case PauseObservation            =>
          Button(Button.Props(icon = Some(IconPause), color = Some("teal"), dataTooltip = Some("Pause the current exposure")))
        case StopObservation             =>
          Button(Button.Props(icon = Some(IconStop), color = Some("orange"), dataTooltip = Some("Stop the current exposure early")))
        case AbortObservation            =>
          Button(Button.Props(icon = Some(IconTrash), color = Some("red"), dataTooltip = Some("Abort the current exposure")))
        case ResumeObservation           =>
          Button(Button.Props(icon = Some(IconPlay), color = Some("blue"), dataTooltip = Some("Resume the current exposure")))
        // Hamamatsu operations
        case PauseImmediatelyObservation =>
          Button(Button.Props(icon = Some(IconPause), color = Some("teal"), dataTooltip = Some("Pause the current exposure immediately")))
        case PauseGracefullyObservation  =>
          Button(Button.Props(icon = Some(IconPause), color = Some("teal"), basic = true, dataTooltip = Some("Pause the current exposure gracefully")))
        case StopImmediatelyObservation  =>
          Button(Button.Props(icon = Some(IconStop), color = Some("orange"), dataTooltip = Some("Stop the current exposure immediately")))
        case StopGracefullyObservation   =>
          Button(Button.Props(icon = Some(IconStop), color = Some("orange"), basic = true, dataTooltip = Some("Stop the current exposure gracefully")))
      }
    }

    def controlButtons(loggedIn: Boolean, sequenceView: SequenceView, step: Step): ReactNode =
      <.div(
        ^.cls := "ui two column grid",
        <.div(
          ^.cls := "ui row",
          <.div(
            ^.cls := "left column",
            <.div(
              ^.cls := "ui segment basic running",
              step.status.shows
            )
          ),
          loggedIn ?= <.div(
            ^.cls := "right column",
            <.div(
              ^.cls := "ui icon buttons",
              observationControlButtons(sequenceView, step)
            )
          )
        )
      )

    def stepInError(loggedIn: Boolean, s: SequenceView, msg: String): ReactNode =
        <.div(
          <.p(s"Error: $msg"),
          loggedIn ?=
            IconMessage(
              IconMessage.Props(IconAttention, None, IconMessage.Style.Info, Size.Tiny),
                s"Press ",
                <.b(s.isPartiallyExecuted ? "Continue" | "Run"),
                " to re-try"
              )
        )

    def stepDisplay(p: Props, step: Step): ReactNode =
      step.status match {
        case StepState.Running | StepState.Paused => controlButtons(p.status.isLogged, p.s, step)
        case StepState.Completed                  => <.p(step.status.shows)
        case StepState.Error(msg)                 => stepInError(p.status.isLogged, p.s, msg)
        // TODO Remove the 2 conditions below when supported by the engine
        case s if step.skip                       => <.p(step.status.shows + " - Skipped")
        case _                                    => <.p(step.status.shows)
      }

    def selectRow(step: Step, index: Int): Callback =
      $.props >>= { p => Callback.when(p.status.isLogged)(Callback.when(step.status.canRunFrom)($.props >>= {_.onStepToRun(index)})) }

    def mouseEnter(index: Int): Callback =
      $.state.flatMap(s => Callback.when(!s.onHover.contains(index))($.modState(_.copy(onHover = Some(index)))))

    def mouseLeave(index: Int): Callback =
      $.state.flatMap(s => Callback.when(s.onHover.contains(index))($.modState(_.copy(onHover = None))))

    def mouseLeave: Callback =
      $.modState(_.copy(onHover = None))

    def markAsSkipped(view: SequenceView, step: Step): Callback =
      $.props >>= {p => Callback.when(p.status.isLogged)(Callback { SeqexecCircuit.dispatch(FlipSkipStep(view, step)) }) }

    def breakpointAt(view: SequenceView, step: Step): Callback =
      $.props >>= { p => Callback.when(p.status.isLogged)(Callback { SeqexecCircuit.dispatch(FlipBreakpointStep(view, step)) }) }

    def stepsTable(p: Props, s: State): TagMod =
      <.table(
        ^.cls := "ui selectable compact celled table unstackable",
        SeqexecStyles.stepsTable,
        ^.onMouseLeave  --> mouseLeave,
        <.thead(
          <.tr(
            TableHeader(TableHeader.Props(collapsing = true, aligned = Aligned.Center, colSpan = Some(2)), IconSettings.copyIcon(key = s"${p.s.metadata.instrument}.steps.settings")),
            TableHeader(TableHeader.Props(collapsing = true), "Step"),
            TableHeader(TableHeader.Props(width = Width.Eight), "State"),
            TableHeader(TableHeader.Props(width = Width.Eight), "File"),
            TableHeader(TableHeader.Props(collapsing = true), "Config")
          )
        ),
        <.tbody(
          SeqexecStyles.stepsListBody,
          p.s.steps.zipWithIndex.map {
            case (step, i) =>
              List(
                <.tr(
                  SeqexecStyles.trNoBorder,
                  SeqexecStyles.trBreakpoint,
                  ^.onMouseOver --> mouseEnter(i),
                  <.td(
                    SeqexecStyles.gutterTd,
                    SeqexecStyles.tdNoPadding,
                    ^.rowSpan := 2,
                    <.div(
                      SeqexecStyles.breakpointHandleContainer,
                      step.canSetBreakpoint ? SeqexecStyles.gutterIconVisible | SeqexecStyles.gutterIconHidden,
                      if (step.breakpoint) {
                        Icon.IconMinus.copyIcon(link = true, color = Some("brown"), onClick = breakpointAt(p.s, step))
                      } else {
                        Icon.IconCaretDown.copyIcon(link = true, color = Some("gray"), onClick = breakpointAt(p.s, step))
                      }
                    ),
                    <.div(
                      SeqexecStyles.skipHandleContainer,
                      if (step.skip) {
                        IconPlusSquareOutline.copyIcon(link = true, extraStyles = List(if (s.onHover.contains(i) && step.canSetSkipmark) SeqexecStyles.gutterIconVisible else SeqexecStyles.gutterIconHidden), onClick = markAsSkipped(p.s, step))
                      } else {
                        IconMinusCircle.copyIcon(link = true, color = Some("orange"), extraStyles = List(if (s.onHover.contains(i) && step.canSetSkipmark) SeqexecStyles.gutterIconVisible else SeqexecStyles.gutterIconHidden), onClick = markAsSkipped(p.s, step))
                      }
                    )
                  ),
                  <.td(
                    if (step.breakpoint) SeqexecStyles.breakpointTrOn else SeqexecStyles.breakpointTrOff,
                    SeqexecStyles.tdNoPadding,
                    ^.colSpan := 5
                  )
                ),
                <.tr(
                  SeqexecStyles.trNoBorder,
                  ^.onMouseOver --> mouseEnter(i),
                  // Available row states: http://semantic-ui.com/collections/table.html#positive--negative
                  ^.classSet(
                    "positive" -> (step.status === StepState.Completed),
                    "warning"  -> (step.status === StepState.Running),
                    "negative" -> (step.status === StepState.Paused),
                    // TODO Show error case
                    "negative" -> step.hasError,
                    "active"   -> (step.status === StepState.Skipped),
                    "disabled" -> step.skip
                  ),
                  step.status == StepState.Running ?= SeqexecStyles.stepRunning,
                  <.td(
                    ^.onDoubleClick --> selectRow(step, i),
                    step.status match {
                      case StepState.Completed       => IconCheckmark
                      case StepState.Running         => IconCircleNotched.copyIcon(loading = true)
                      case StepState.Error(_)        => IconAttention
                      case _ if i == p.nextStepToRun => IconChevronRight
                      case _ if step.skip            => IconReply.copyIcon(rotated = Icon.Rotated.CounterClockwise)
                      case _                         => iconEmpty
                    }
                  ),
                  <.td(
                    ^.onDoubleClick --> selectRow(step, i),
                    i + 1),
                  <.td(
                    ^.onDoubleClick --> selectRow(step, i),
                    ^.cls := "middle aligned",
                    stepDisplay(p, step)),
                  <.td(
                    ^.onDoubleClick --> selectRow(step, i),
                    ^.cls := "middle aligned",
                    stepProgress(step)),
                  <.td(
                    ^.cls := "collapsing right aligned",
                    IconCaretRight.copyIcon(onClick = displayStepDetails(p.s, i))
                  )
                )
              )
          }
        )
      )

    def render(p: Props, s: State): ReactTagOf[Div] = {
      <.div(
        ^.cls := "ui row scroll pane",
        SeqexecStyles.stepsListPane,
        ^.ref := scrollRef,
        p.stepConfigDisplayed.map { i =>
          // TODO consider the failure case
          val step = p.s.steps(i)
          configTable(step)
        }.getOrElse {
          stepsTable(p, s)
        }
      )
    }
  }

  def requestPause(s: SequenceView): Callback = Callback {SeqexecCircuit.dispatch(RequestPause(s))}

  def displayStepDetails(s: SequenceView, i: Int): Callback = Callback {SeqexecCircuit.dispatch(ShowStep(s, i))}

  // Reference to the specifc DOM marked by the name `scrollRef`
  private val scrollRef = Ref[HTMLElement]("scrollRef")

  val component = ReactComponentB[Props]("StepsTable")
    .initialState(State(0, None, autoScrolled = false))
    .renderBackend[Backend]
    .componentWillReceiveProps { f =>
      // Override the manually selected step to run if the state changes
      val nextStepToRunCB = Callback.empty
        //Callback.when(f.nextProps.s.status != f.currentProps.s.status)(f.$.modState(_.copy(nextStepToRun = f.nextProps.s.nextStepToRun.getOrElse(0))))

      // Called when the props have changed. At this time we can recalculate
      // if the scroll position needs to be updated and store it in the State
      val div = scrollRef(f.$)
      val scrollStateCB = if (f.nextProps.s.id =/= f.currentProps.s.id) {
        // It will reset to 0 if the sequence changes
        // TODO It may be better to remember the pos of executed steps per sequence
        f.$.modState(_.copy(nextScrollPos = 0, autoScrolled = true))
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
                case _ if el.parentNode == document.body                                       =>
                  top <= document.documentElement.clientHeight
                case _: Element                                                                =>
                  go(el.parentNode)
              }

            go(el.parentNode)
          }

          /**
            * Calculates the new scroll position if the relevant row is not visible
            */
          def scrollPosition: Option[Double] = {
            // Build a css selector for the relevant row, either the last one when complete
            // or the currently running one
            val rowSelector = if (f.nextProps.s.allStepsDone) {
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
            f.$.modState(_.copy(nextScrollPos = p, autoScrolled = true))
          }
        }
      }
      // Run both callbacks, to update the runRequested state and the scroll position
      scrollStateCB *> nextStepToRunCB
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

  def apply(p: Props) = component(p)
}

