// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.web.client.ModelOps._
import edu.gemini.seqexec.web.client.model.Pages.{SeqexecPages, SequenceConfigPage}
import edu.gemini.seqexec.web.client.actions.{NavigateSilentTo, FlipBreakpointStep, FlipSkipStep}
import edu.gemini.seqexec.web.client.circuit.{ClientStatus, SeqexecCircuit, StepsTableFocus}
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.components.sequence.steps.OffsetFns._
import edu.gemini.seqexec.web.client.lenses.stepTypeO
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import edu.gemini.seqexec.web.client.semanticui.elements.table.TableHeader
import edu.gemini.seqexec.web.client.services.HtmlConstants.iconEmpty
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html.Div

import scalacss.ScalaCssReact._
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import scalaz.syntax.show._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._

/**
 * Component to wrap the progress bar
 */
object ObservationProgressBar {
  private val component = ScalaComponent.builder[ImageFileId]("ObservationProgressBar")
    .stateless
    .render_P(fileId =>
      <.div(
        ^.cls := "ui small top aligned progress vcentered",
        <.div(
          ^.cls := "bar",
          <.div(
            ^.cls := "progress")
        ),
        <.div(
          ^.cls := "label",
          SeqexecStyles.componentLabel,
          fileId
        )
      )
    )
    .build

  def apply(p: ImageFileId): Unmounted[ImageFileId, Unit, Unit] = component(p)
}

/**
 * Headers of the steps table
 */
object StepsTableHeader {
  private val component = ScalaComponent.builder[OffsetsDisplay]("StepsTableHeader")
    .stateless
    .render_P { p =>
      val displayOffsets = p === OffsetsDisplay.NoDisplay
      val stateWidth = displayOffsets.fold(Width.Three, Width.Four)
      <.thead(
        <.tr(
          TableHeader(TableHeader.Props(collapsing = true, aligned = Aligned.Center, colSpan = Some(2)), IconSettings),
          TableHeader(TableHeader.Props(collapsing = true), "Step"),
          TableHeader(TableHeader.Props(width = stateWidth), "State"),
          TableHeader(TableHeader.Props(width = Width.Three), "Offset").unless(displayOffsets),
          TableHeader(TableHeader.Props(width = Width.One), "Guiding"),
          TableHeader(TableHeader.Props(width = Width.One), "Exposure"),
          TableHeader(TableHeader.Props(width = Width.Two), "Filter"),
          TableHeader(TableHeader.Props(width = Width.Two), "FPU"),
          TableHeader(TableHeader.Props(width = Width.Two, aligned = Aligned.Right), "Type"),
          TableHeader(TableHeader.Props(collapsing = true, width = Width.Eight), "Progress"),
          TableHeader(TableHeader.Props(collapsing = true), "Config")
        )
      )
    }
    .build

  def apply(p: OffsetsDisplay): Unmounted[OffsetsDisplay, Unit, Unit] = component(p)
}

object StepConfigTable {
  final case class Props(step: Step)

  private val component = ScalaComponent.builder[Props]("StepConfigTable")
    .stateless
    .render_P ( p =>
      <.table(
        ^.cls := "ui selectable compact celled table unstackable",
        <.thead(
          <.tr(
            TableHeader(TableHeader.Props(collapsing = true), "Name"),
            TableHeader(TableHeader.Props(width = Width.Six), "Value")
          )
        ),
        <.tbody(
          p.step.config.flatMap {
            case (sub, c) =>
              c.map {
                case (k, v) =>
                  <.tr(
                    ^.classSet(
                      "positive" -> (sub === SystemName.instrument),
                      "warning"  -> (sub === SystemName.telescope)
                    ),
                    SeqexecStyles.observeConfig.when(k.startsWith("observe")),
                    SeqexecStyles.observeConfig.when(k.startsWith("ocs")),
                    <.td(k),
                    <.td(v)
                  )
              }
          }.toSeq.toTagMod
        )
      )
    ).build

  def apply(s: Step): Unmounted[Props, Unit, Unit] = component(Props(s))
}
/**
  * Container for a table with the steps
  */
object StepsTableContainer {
  final case class State(nextScrollPos  : Double,
                   onHover        : Option[Int],
                   autoScrolled   : Boolean)

  final case class Props(router: RouterCtl[SeqexecPages], stepsTable: ModelProxy[(ClientStatus, Option[StepsTableFocus])], onStepToRun: Int => Callback) {
    def status: ClientStatus = stepsTable()._1
    def steps: Option[StepsTableFocus] = stepsTable()._2
    private val stepsList: List[Step] = ~steps.map(_.steps)
    // Find out if offsets should be displayed
    val offsetsDisplay: OffsetsDisplay = stepsList.offsetsDisplay
  }

  class Backend($: BackendScope[Props, State]) {

    def labelColor(status: ActionStatus): String = status match {
      case ActionStatus.Pending   => "gray"
      case ActionStatus.Running   => "yellow"
      case ActionStatus.Completed => "green"
      case ActionStatus.Paused    => "orange"
      case ActionStatus.Failed    => "red"
    }

    def labelIcon(status: ActionStatus): Option[Icon] = status match {
      case ActionStatus.Pending   => None
      case ActionStatus.Running   => IconCircleNotched.copyIcon(loading = true).some
      case ActionStatus.Completed => IconCheckmark.some
      case ActionStatus.Paused    => IconPause.some
      case ActionStatus.Failed    => IconStopCircle.some
    }

    def statusLabel(system: Resource, status: ActionStatus): VdomNode =
      Label(Label.Props(s"${system.shows}", color = labelColor(status).some, icon = labelIcon(status)))

    def stepSystemsStatus(step: Step): VdomNode =
      step match {
        case StandardStep(_, _, _, _, _, _, configStatus, _) =>
          <.div(configStatus.map(Function.tupled(statusLabel)).toTagMod)
        case _ =>
          <.div(step.status.shows)
      }

    def stepProgress(state: SequenceState, step: Step): VdomNode =
      (state, step.status) match {
        case (SequenceState.Pausing, StepState.Running) =>
          <.div(state.shows)
        case (SequenceState.Stopping, _) =>
          <.div(step.status.shows)
        case (_, StepState.Pending) =>
          step.fileId.fold(<.div("Pending"))(_ => <.div("Configuring"))
        case (_, StepState.Running) =>
          step.fileId.fold(<.div(stepSystemsStatus(step)): VdomNode)(fileId => ObservationProgressBar(fileId): VdomNode)
        case (_, StepState.Completed) =>
          step.fileId.getOrElse(""): String
        case _ =>
          step.file.getOrElse(""): String
      }


    def controlButtons(loggedIn: Boolean, p: StepsTableFocus, step: Step): VdomNode =
      StepsControlButtonsWrapper(StepsControlButtonsWrapper.Props(loggedIn, p, step))

    def isPartiallyExecuted(p: StepsTableFocus): Boolean =
      p.steps.exists(_.status === StepState.Completed)

    def stepInError(loggedIn: Boolean, isPartiallyExecuted: Boolean, msg: String): VdomNode =
      <.div(
        <.p(s"Error: $msg"),
        IconMessage(
          IconMessage.Props(IconAttention, None, IconMessage.Style.Info, Size.Tiny),
            s"Press ",
            <.b(isPartiallyExecuted ? "Continue" | "Run"),
            " to re-try"
        ).when(loggedIn)
      )

    def stepDisplay(status: ClientStatus, p: StepsTableFocus, state: SequenceState, step: Step): VdomNode =
      (state, step.status) match {
        case (SequenceState.Pausing, StepState.Running) => <.p(state.shows)
        case (_, StepState.Running | StepState.Paused)  => controlButtons(status.isLogged, p, step)
        case (_, StepState.Completed)                   => <.p(step.status.shows)
        case (_, StepState.Error(msg))                  => stepInError(status.isLogged, isPartiallyExecuted(p), msg)
        // TODO Remove the 2 conditions below when supported by the engine
        case (_, s) if step.skip                        => <.p("Skipped")
        case (_, _)                                     => <.p(step.status.shows)
      }

    def selectRow(step: Step, index: Int): Callback =
      $.props >>= { p => Callback.when(p.status.isLogged)(Callback.when(step.status.canRunFrom)($.props >>= {_.onStepToRun(index)})) }

    def mouseEnter(index: Int): Callback =
      $.state.flatMap(s => Callback.when(!s.onHover.contains(index))($.modState(_.copy(onHover = Some(index)))))

    def mouseLeaveOn(index: Int): Callback =
      $.state.flatMap(s => Callback.when(s.onHover.contains(index))($.modState(_.copy(onHover = None))))

    def mouseLeave: Callback =
      $.modState(_.copy(onHover = None))

    def markAsSkipped(id: SequenceId, step: Step): Callback =
      $.props >>= { p => Callback.when(p.status.isLogged)(p.stepsTable.dispatchCB(FlipSkipStep(id, step))) }

    def breakpointAt(id: SequenceId, step: Step): Callback =
      $.props >>= { p => Callback.when(p.status.isLogged)(p.stepsTable.dispatchCB(FlipBreakpointStep(id, step))) }

    private def gutterCol(id: SequenceId, i: Int, step: Step, s: State) =
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
              Icon.IconMinus.copyIcon(link = true, color = Some("brown"), onClick = breakpointAt(id, step))
            } else {
              Icon.IconCaretDown.copyIcon(link = true, color = Some("grey"), onClick = breakpointAt(id, step))
            }
          ),
          <.div(
            SeqexecStyles.skipHandleContainer,
            if (step.skip) {
              IconPlusSquareOutline.copyIcon(link = true, extraStyles = List(if (s.onHover.contains(i) && step.canSetSkipmark) SeqexecStyles.gutterIconVisible else SeqexecStyles.gutterIconHidden), onClick = markAsSkipped(id, step))
            } else {
              IconMinusCircle.copyIcon(link = true, color = Some("orange"), extraStyles = List(if (s.onHover.contains(i) && step.canSetSkipmark) SeqexecStyles.gutterIconVisible else SeqexecStyles.gutterIconHidden), onClick = markAsSkipped(id, step))
            }
          )
        ),
        <.td(
          if (step.breakpoint) SeqexecStyles.breakpointTrOn else SeqexecStyles.breakpointTrOff,
          SeqexecStyles.tdNoPadding,
          ^.colSpan := 11
        )
      )

    private def stepIcon(p: StepsTableFocus, step: Step, i: Int): VdomNode =
      step.status match {
        case StepState.Completed                  => IconCheckmark
        case StepState.Running                    => IconCircleNotched.copyIcon(loading = true)
        case StepState.Error(_)                   => IconAttention
        case _ if p.nextStepToRun.forall(_ === i) => IconChevronRight
        case _ if step.skip                       => IconReply.copyIcon(rotated = Icon.Rotated.CounterClockwise)
        case _                                    => iconEmpty
      }

    private def classSet(step: Step): List[(String, Boolean)] = List(
      "disabled" -> (step.skip || step.status === StepState.Completed),
      "warning"  -> (step.status === StepState.Running),
      "negative" -> (step.status === StepState.Paused),
      "negative" -> step.hasError,
      "active"   -> (step.status === StepState.Skipped)
    )

    private def stepTypeLabel(step: Step): Option[Unmounted[Label.Props, Unit, Unit]] =
      stepTypeO.getOption(step).map { st =>
        val stepTypeColor = st match {
          case _ if step.status === StepState.Completed => "light gray"
          case StepType.Object                          => "green"
          case StepType.Arc                             => "violet"
          case StepType.Flat                            => "grey"
          case StepType.Bias                            => "teal"
          case StepType.Dark                            => "black"
          case StepType.Calibration                     => "blue"
        }
        Label(Label.Props(st.shows, color = stepTypeColor.some, size = Size.Small))
      }

    private def stepIconCell(p: StepsTableFocus, step: Step, i: Int) =
      <.td( // Column step icon
        ^.onDoubleClick --> selectRow(step, i),
        <.div(
          ^.cls := "center aligned",
          stepIcon(p, step, i)
        )
      )

    private def stepNumberCell(step: Step, i: Int) =
      <.td( // Column step number
        ^.onDoubleClick --> selectRow(step, i),
        SeqexecStyles.componentLabel,
        i + 1
      )

    private def stepStatusCell(status: ClientStatus, p: StepsTableFocus, step: Step, state: SequenceState, i: Int) =
      <.td( // Column step status
        ^.onDoubleClick --> selectRow(step, i),
        ^.cls := "middle aligned",
        SeqexecStyles.componentLabel,
        stepDisplay(status, p, state, step)
      )

    private def offsetDisplayCell(offsetsDisplay: OffsetsDisplay, step: Step, i: Int) =
      offsetsDisplay match {
        case OffsetsDisplay.DisplayOffsets(offsetWidth) =>
          <.td( // Column step offset
            ^.onDoubleClick --> selectRow(step, i),
            OffsetBlock(OffsetBlock.Props(step, offsetWidth))
          )
        case _ => EmptyVdom
      }

    private def stepGuidingCell(step: Step, i: Int) =
      <.td( // Column step guiding
        ^.onDoubleClick --> selectRow(step, i),
        GuidingBlock(GuidingBlock.Props(step))
      )

    private def stepExposureTimeCell(instrument: Instrument, step: Step, i: Int) =
      <.td( // Column exposure time
        ^.onDoubleClick --> selectRow(step, i),
        ExposureTime(ExposureTime.Props(step, instrument))
      )

    private def stepFilterCell(instrument: Instrument, step: Step, i: Int) =
      <.td( // Column object type
        ^.onDoubleClick --> selectRow(step, i),
        FilterCell(FilterCell.Props(step, instrument))
      )

    private def stepFPUCell(instrument: Instrument, step: Step, i: Int) =
      <.td( // Column object type
        ^.onDoubleClick --> selectRow(step, i),
        FPUCell(FPUCell.Props(step, instrument))
      )

    private def stepObjectTypeCell(step: Step, i: Int) =
      <.td( // Column object type
        ^.onDoubleClick --> selectRow(step, i),
        ^.cls := "right aligned",
        stepTypeLabel(step).whenDefined
      )

    private def stepProgressCell(step: Step, state: SequenceState, i: Int) =
      <.td( // Column progress
        ^.onDoubleClick --> selectRow(step, i),
        ^.classSet(
          "top aligned"    -> step.isObserving,
          "center aligned" -> !step.isObserving
        ),
        SeqexecStyles.componentLabel,
        stepProgress(state, step)
      )

    private def stepDetailsCell(router: RouterCtl[SeqexecPages], instrument: Instrument, id: SequenceId, i: Int) =
      <.td( // Column link to details
        ^.cls := "collapsing right aligned",
        router.link(SequenceConfigPage(instrument, id, i + 1))
          (IconCaretRight.copyIcon(color = "black".some, onClick = displayStepDetails(instrument, id, i)))
      )

    private def stepCols(router: RouterCtl[SeqexecPages], status: ClientStatus, p: StepsTableFocus, i: Int, state: SequenceState, step: Step, offsetsDisplay: OffsetsDisplay) =
      <.tr(
        SeqexecStyles.trNoBorder,
        ^.onMouseOver --> mouseEnter(i),
        ^.classSet(classSet(step): _*),
        SeqexecStyles.stepRunning.when(step.status === StepState.Running),
        stepIconCell(p, step, i),
        stepNumberCell(step, i),
        stepStatusCell(status, p, step, state, i),
        offsetDisplayCell(offsetsDisplay, step, i),
        stepGuidingCell(step, i),
        stepExposureTimeCell(p.instrument, step, i),
        stepFilterCell(p.instrument, step, i),
        stepFPUCell(p.instrument, step, i),
        stepObjectTypeCell(step, i),
        stepProgressCell(step, state, i),
        stepDetailsCell(router, p.instrument, p.id, i)
      )

    def stepsTable(props: Props, p: StepsTableFocus, s: State): TagMod =
      <.table(
        ^.cls := "ui selectable compact celled table unstackable",
        SeqexecStyles.stepsTable,
        ^.onMouseLeave  --> mouseLeave,
        StepsTableHeader(props.offsetsDisplay),
        <.tbody(
          SeqexecStyles.stepsListBody,
          p.steps.zipWithIndex.flatMap {
            case (step, i) =>
              List(
                gutterCol(p.id, i, step, s),
                stepCols(props.router, props.status, p, i, p.state, step, props.offsetsDisplay)
              )
          }.toTagMod
        )
      )

    def render(p: Props, s: State): VdomTagOf[Div] = {
      <.div(
        ^.cls := "ui row scroll pane",
        SeqexecStyles.stepsListPane,
        //^.ref := scrollRef,
        p.steps.whenDefined { tab =>
          tab.stepConfigDisplayed.map { i =>
            val step = tab.steps(i)
            StepConfigTable(step): TagMod
          }.getOrElse {
            stepsTable(p, tab, s)
          }
        }
      )
    }
  }

  def displayStepDetails(i: Instrument, id: SequenceId, step: Int): Callback =
    Callback{ SeqexecCircuit.dispatch(NavigateSilentTo(SequenceConfigPage(i, id, step + 1))) }

  // Reference to the specifc DOM marked by the name `scrollRef`
  //private val scrollRef = Ref[HTMLElement]("scrollRef")

  private val component = ScalaComponent.builder[Props]("StepsTable")
    .initialState(State(0, None, autoScrolled = false))
    .renderBackend[Backend]
    .componentWillReceiveProps { f =>
      // Override the manually selected step to run if the state changes
      val nextStepToRunCB = Callback.empty
        //Callback.when(f.nextProps.s.status != f.currentProps.s.status)(f.$.modState(_.copy(nextStepToRun = f.nextProps.s.nextStepToRun.getOrElse(0))))

      // Called when the props have changed. At this time we can recalculate
      // if the scroll position needs to be updated and store it in the State
      /*val div = scrollRef(f.$)
      val scrollStateCB = if (f.nextProps.s.id =/= f.currentProps.s.id) {
        // It will reset to 0 if the sequence changes
        // TODO It may be better to remember the pos of executed steps per sequence
        f.modState(_.copy(nextScrollPos = 0, autoScrolled = true))
      } else {
        div.fold(Callback.empty) { scrollPane =>
          **
            * Calculates if the element is visible inside the scroll pane up the dom tree
            *
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

          **
            * Calculates the new scroll position if the relevant row is not visible
            *
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
      }*/
      // Run both callbacks, to update the runRequested state and the scroll position
      /*scrollStateCB *> */nextStepToRunCB
    }.componentWillUpdate { f =>
      // Called before the DOM is rendered on the updated props. This is the chance
      // to update the scroll position if needed
      /*val div = scrollRef(f.$)
      div.fold(Callback.empty){ scrollPane =>
        // If the state indicates to scroll, update the scroll position
        Callback.when(f.nextState.autoScrolled)(Callback {
            scrollPane.scrollTop = f.nextState.nextScrollPos
          }
        )
      }*/
      Callback.empty
    }.build

  def apply(p: Props): Unmounted[Props, State, Backend] = component(p)
}
