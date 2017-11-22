// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.ScalazReact._
import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.table.TableHeader
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.web.client.circuit.{SeqexecCircuit, ClientStatus, StepsTableFocus}
import edu.gemini.seqexec.web.client.actions.{FlipSkipStep, FlipBreakpointStep, ShowStep}
import edu.gemini.seqexec.web.client.lenses._
import edu.gemini.seqexec.web.client.ModelOps._
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.services.HtmlConstants.iconEmpty
import edu.gemini.web.client.utils._

import scalacss.ScalaCssReact._
import scalaz.syntax.show._
import scalaz.syntax.order._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.std.AllInstances._
import org.scalajs.dom.html.Div
import org.scalajs.dom.CanvasRenderingContext2D

/**
 * Utility methods to display offsets and calculate their widths
 */
trait OffsetFns {
  def offsetAxis(axis: OffsetAxis): String =
    f"${axis.shows}:"

  def offsetValueFormat(off: Offset): String =
    f" ${off.value}%003.2f″"

  def tableTextWidth(text: String): Int = textWidth(text, "bold 14px sans-serif")

  def offsetText(axis: OffsetAxis)(step: Step): String =
    offsetValueFormat(axis match {
      case OffsetAxis.AxisP => telescopeOffsetPO.getOption(step).getOrElse(TelescopeOffset.P.Zero)
      case OffsetAxis.AxisQ => telescopeOffsetQO.getOption(step).getOrElse(TelescopeOffset.Q.Zero)
    })

  private val offsetPText = offsetText(OffsetAxis.AxisP) _
  private val offsetQText = offsetText(OffsetAxis.AxisQ) _

  val pLabelWidth: Int = tableTextWidth(offsetAxis(OffsetAxis.AxisP))
  val qLabelWidth: Int = tableTextWidth(offsetAxis(OffsetAxis.AxisQ))

  // Calculate the widest offset step
  def sequenceOffsetWidths(steps: List[Step]): (Int, Int) = {
    steps.map(s => (tableTextWidth(offsetPText(s)), tableTextWidth(offsetQText(s)))).foldLeft((0, 0)) {
      case ((p1, q1), (p2, q2)) => (p1.max(p2), q1.max(q2))
    }
  }
}

/**
  * Container for a table with the steps
  */
object StepsTableContainer extends OffsetFns {
  final case class State(nextScrollPos  : Double,
                   onHover        : Option[Int],
                   autoScrolled   : Boolean)

  final case class Props(stepsTable: ModelProxy[(ClientStatus, Option[StepsTableFocus])], onStepToRun: Int => Callback) {
    def status: ClientStatus = stepsTable()._1
    def steps: Option[StepsTableFocus] = stepsTable()._2
    val offsetsWidth: Int = {
      val (p, q) = sequenceOffsetWidths(~steps.map(_.steps))
      scala.math.max(p, q)
    }
  }

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
          step.config.flatMap {
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

    def labelColor(status: ActionStatus): String = status match {
      case ActionStatus.Pending   => "gray"
      case ActionStatus.Running   => "yellow"
      case ActionStatus.Completed => "green"
    }

    def labelIcon(status: ActionStatus): Option[Icon] = status match {
      case ActionStatus.Pending   => None
      case ActionStatus.Running   => IconCircleNotched.copyIcon(loading = true).some
      case ActionStatus.Completed => IconCheckmark.some
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
          step.fileId.fold(<.div(stepSystemsStatus(step)))(fileId =>
            <.div(
              ^.cls := "ui small progress vcentered",
              <.div(
                ^.cls := "bar",
                <.div(
                  ^.cls := "progress")
              ),
              <.div(
                ^.cls := "label",
                fileId
              )
            )
          )
        case (_, StepState.Completed) =>
          step.fileId.getOrElse(""): String
        case _ =>
          step.file.getOrElse(""): String
      }


    def controlButtons(loggedIn: Boolean, p: StepsTableFocus, step: Step): VdomNode =
      <.div(
        ^.cls := "ui two column grid stackable",
        <.div(
          ^.cls := "ui row",
          <.div(
            ^.cls := "left column five wide left floated",
            <.div(
              ^.cls := "ui segment basic running",
              step.shows
            )
          ),
          <.div(
            ^.cls := "right floated right aligned eleven wide computer sixteen wide tablet only",
            SeqexecStyles.buttonsRow,
            StepsControlButtons(p.id, p.instrument, p.state, step).when(step.isObserving)
          ).when(loggedIn && p.state === SequenceState.Running)
        )
      )

    def isPartiallyExecuted(p: StepsTableFocus): Boolean = p.steps.exists(_.status === StepState.Completed)

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
        case (_, s) if step.skip                        => <.p(step.status.shows + " - Skipped")
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
      $.props >>= {p => Callback.when(p.status.isLogged)(p.stepsTable.dispatchCB(FlipSkipStep(id, step))) }

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
          ^.colSpan := 5
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

    private def stepCols(status: ClientStatus, p: StepsTableFocus, i: Int, state: SequenceState, step: Step, offsetWidth: Int) =
      <.tr(
        SeqexecStyles.trNoBorder,
        ^.onMouseOver --> mouseEnter(i),
        ^.classSet(
          "positive" -> (step.status === StepState.Completed),
          "warning"  -> (step.status === StepState.Running),
          "negative" -> (step.status === StepState.Paused),
          "negative" -> step.hasError,
          "active"   -> (step.status === StepState.Skipped),
          "disabled" -> step.skip
        ),
        SeqexecStyles.stepRunning.when(step.status === StepState.Running),
        <.td(
          ^.onDoubleClick --> selectRow(step, i),
          stepIcon(p, step, i)
        ),
        <.td(
          ^.onDoubleClick --> selectRow(step, i),
          i + 1
        ),
        <.td(
          ^.onDoubleClick --> selectRow(step, i),
          ^.cls := "middle aligned",
          stepDisplay(status, p, state, step)
        ),
        <.td(
          ^.onDoubleClick --> selectRow(step, i),
          ^.cls := "right aligned",
          SeqexecStyles.tdNoUpDownPadding,
          StepSettings(StepSettings.Props(step, offsetWidth))
        ),
        <.td(
          ^.onDoubleClick --> selectRow(step, i),
          ^.classSet(
            "top aligned"    -> step.isObserving,
            "middle aligned" -> !step.isObserving
          ),
          stepProgress(state, step)
        ),
        <.td(
          ^.cls := "collapsing right aligned",
          IconCaretRight.copyIcon(onClick = displayStepDetails(p.id, i))
        )
      )

    def stepsTable(status: ClientStatus, p: StepsTableFocus, s: State, offsetWidth: Int): TagMod =
      <.table(
        ^.cls := "ui selectable compact celled table unstackable",
        SeqexecStyles.stepsTable,
        ^.onMouseLeave  --> mouseLeave,
        <.thead(
          <.tr(
            TableHeader(TableHeader.Props(collapsing = true, aligned = Aligned.Center, colSpan = Some(2)), IconSettings),
            TableHeader(TableHeader.Props(collapsing = true), "Step"),
            TableHeader(TableHeader.Props(width = Width.Four), "State"),
            TableHeader(TableHeader.Props(width = Width.Eight), "Settings"),
            TableHeader(TableHeader.Props(width = Width.Four), "Progress"),
            TableHeader(TableHeader.Props(collapsing = true), "Config")
          )
        ),
        <.tbody(
          SeqexecStyles.stepsListBody,
          p.steps.zipWithIndex.flatMap {
            case (step, i) =>
              List(
                gutterCol(p.id, i, step, s),
                stepCols(status, p, i, p.state, step, offsetWidth)
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
            configTable(step)
          }.getOrElse {
            stepsTable(p.status, tab, s, p.offsetsWidth)
          }
        }
      )
    }
  }

  def displayStepDetails(s: SequenceId, i: Int): Callback = Callback {SeqexecCircuit.dispatch(ShowStep(s, i))}

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

object OffsetGrid {
  private val Size = 33.0
  final case class Props(p: TelescopeOffset.P, q: TelescopeOffset.Q)
  final case class State(canvas: Option[Canvas])

  private val ST = ReactS.Fix[State]

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def render(props: Props, state: State): Callback = state.canvas.fold(Callback.empty) { c => Callback {
    // The canvas API is very imperative and stateful but we are inside a Callback!
    val ctx = c.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    c.width = Size.toInt
    c.height = Size.toInt
    // First quadrant
    ctx.fillStyle = if (props.p > TelescopeOffset.P.Zero && props.q > TelescopeOffset.Q.Zero) "darkgray" else "white"
    ctx.fillRect(0, 0, Size / 2, Size / 2)
    // Second quadrant
    ctx.fillStyle = if (props.p < TelescopeOffset.P.Zero && props.q > TelescopeOffset.Q.Zero) "darkgray" else "white"
    ctx.fillRect(Size / 2, 0, Size / 2, Size / 2)
    // Third quadrant
    ctx.fillStyle = if (props.p < TelescopeOffset.P.Zero && props.q < TelescopeOffset.Q.Zero) "darkgray" else "white"
    ctx.fillRect(Size / 2, Size / 2, Size / 2, Size / 2)
    // Fourth quadrant
    ctx.fillStyle = if (props.p > TelescopeOffset.P.Zero && props.q < TelescopeOffset.Q.Zero) "darkgray" else "white"
    ctx.fillRect(0, Size / 2, Size / 2, Size / 2)
    // Grid
    ctx.fillStyle = "black"
    // Outer border
    ctx.strokeRect(0, 0, Size, Size)
    // Inner borders
    ctx.strokeRect(0, 0, Size / 2, Size / 2)
    ctx.strokeRect(Size / 2, 0, Size / 2, Size / 2)
    ctx.strokeRect(0, Size / 2, Size / 2, Size / 2)
    ctx.strokeRect(Size/2, Size / 2, Size / 2, Size / 2)
  }}

  private val component = ScalaComponent.builder[Props]("OffsetGrid")
    .initialState(State(None))
    .render_P ( p =>
      <.canvas(
        ^.cls := "middle aligned",
        ^.width := Size.toInt.px,
        ^.height := Size.toInt.px
      )
    ).componentWillReceiveProps { ctx =>
      render(ctx.nextProps, ctx.state)
    }.componentDidMount { ctx =>
      // Grab a copy of the canvas
      val state = State(Some(ctx.getDOMNode.domCast[Canvas]))
      ctx.runState(ST.set(state)) >> render(ctx.props, state)
    }.build

  def apply(p: Props): Unmounted[Props, State, Unit] = component(p)

}

/**
 * Component to display the settings of a given step
 */
object StepSettings extends OffsetFns {
  final case class Props(s: Step, offsetWidth: Int)
  private val component = ScalaComponent.builder[Props]("StepSettings")
    .stateless
    .render_P { p =>
      val offsetP = telescopeOffsetPO.getOption(p.s).getOrElse(TelescopeOffset.P.Zero)
      val offsetQ = telescopeOffsetQO.getOption(p.s).getOrElse(TelescopeOffset.Q.Zero)
      val stepTypeLabel = stepTypeO.getOption(p.s).map { st =>
        val stepTypeColor = st match {
          case StepType.Object      => "green"
          case StepType.Arc         => "violet"
          case StepType.Flat        => "grey"
          case StepType.Bias        => "teal"
          case StepType.Dark        => "black"
          case StepType.Calibration => "blue"
        }
        Label(Label.Props(st.shows, color = stepTypeColor.some))
      }
      <.div(
        ^.cls := "ui two column grid",
        <.div(
          ^.cls := "row",
          <.div(
            ^.cls := "left floated middle aligned one wide column",
            OffsetGrid(OffsetGrid.Props(offsetP, offsetQ))
          ),
          <.div(
            ^.cls := "left floated four wide column",
            <.div(
              ^.cls := "right aligned",
              <.div(
                ^.width := pLabelWidth.px,
                SeqexecStyles.inlineBlock,
                offsetAxis(OffsetAxis.AxisP)
              ),
              <.div(
                ^.width := p.offsetWidth.px,
                SeqexecStyles.inlineBlock,
                offsetValueFormat(offsetP)
              )
            ),
            <.div(
              ^.cls := "right aligned",
              <.div(
                ^.width := qLabelWidth.px,
                SeqexecStyles.inlineBlock,
                offsetAxis(OffsetAxis.AxisQ)
              ),
              <.div(
                ^.width := p.offsetWidth.px,
                SeqexecStyles.inlineBlock,
                offsetValueFormat(offsetQ)
              )
            )
          ),
          <.div(
            ^.cls := "middle aligned right floated four wide column",
            <.div(stepTypeLabel.whenDefined)
          )
        )
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
