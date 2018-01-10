// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.model.operations.ObservationOperations._
import edu.gemini.seqexec.model.operations._
import edu.gemini.seqexec.web.client.ModelOps._
import edu.gemini.seqexec.web.client.actions.{RequestAbort, RequestObsPause, RequestObsResume, RequestStop}
import edu.gemini.seqexec.web.client.circuit.{SeqexecCircuit, StepsTableFocus}
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconPause, IconPlay, IconStop, IconTrash}
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CallbackTo, ScalaComponent, ScalazReact}

import scalacss.ScalaCssReact._
import scalaz.syntax.show._
import scalaz.syntax.std.boolean._

/**
 * Component to wrap the steps control buttons
 */
object StepsControlButtonsWrapper {
  final case class Props(loggedIn: Boolean, p: StepsTableFocus, step: Step, userStopRequested: Boolean)
  private val component = ScalaComponent.builder[Props]("StepsControlButtonsWrapper")
    .stateless
    .render_P(props =>
      <.div(
        ^.cls := "ui two column grid stackable",
        <.div(
          ^.cls := "ui row",
          <.div(
            ^.cls := "left column five wide left floated",
            <.div(
              ^.cls := "ui segment basic running",
              // We need both sequence state and step state to decide what to display
              props.userStopRequested.fold("Stopping...", props.step.shows)
            )
          ),
          <.div(
            ^.cls := "right floated right aligned eleven wide computer sixteen wide tablet only",
            SeqexecStyles.buttonsRow,
            StepsControlButtons(props.p.id, props.p.instrument, props.p.state, props.step).when(props.step.isObserving || props.step.isObservePaused)
          ).when(props.loggedIn && props.p.state.isRunning)
        )
      )
    )
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
 * Contains the control buttons like stop/abort at the row level
 */
object StepsControlButtons {
  final case class Props(id: SequenceId, instrument: Instrument, sequenceState: SequenceState, step: Step)
  final case class State(stopRequested: Boolean, abortRequested: Boolean, pauseRequested: Boolean, resumeRequested: Boolean) {
    val canPause: Boolean = !stopRequested && !abortRequested
    val canAbort: Boolean = !stopRequested && !pauseRequested && !resumeRequested
    val canStop: Boolean = !abortRequested && !pauseRequested && !resumeRequested
    val canResume: Boolean = canPause
  }

  val StopRequested: State = State(stopRequested = true, abortRequested = false, pauseRequested = false, resumeRequested = false)
  val AbortRequested: State = State(stopRequested = false, abortRequested = true, pauseRequested = false, resumeRequested = false)
  val PauseRequested: State = State(stopRequested = false, abortRequested = false, pauseRequested = true, resumeRequested = false)
  val ResumeRequested: State = State(stopRequested = false, abortRequested = false, pauseRequested = false, resumeRequested = true)
  val NoneRequested: State = State(stopRequested = false, abortRequested = false, pauseRequested = false, resumeRequested = false)

  private val ST = ReactS.Fix[State]

  def requestStop(id: SequenceId, stepId: Int): Callback =
    Callback(SeqexecCircuit.dispatch(RequestStop(id, stepId)))

  def requestAbort(id: SequenceId, stepId: Int): Callback =
    Callback(SeqexecCircuit.dispatch(RequestAbort(id, stepId)))

  def requestObsPause(id: SequenceId, stepId: Int): Callback =
    Callback(SeqexecCircuit.dispatch(RequestObsPause(id, stepId)))

  def requestObsResume(id: SequenceId, stepId: Int): Callback =
    Callback(SeqexecCircuit.dispatch(RequestObsResume(id, stepId)))

  def handleStop(id: SequenceId, stepId: Int): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(requestStop(id, stepId)) >> ST.set(StopRequested).liftCB

  def handleAbort(id: SequenceId, stepId: Int): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(requestAbort(id, stepId)) >> ST.set(AbortRequested).liftCB

  def handleObsPause(id: SequenceId, stepId: Int): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(requestObsPause(id, stepId)) >> ST.set(PauseRequested).liftCB

  def handleObsResume(id: SequenceId, stepId: Int): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(requestObsResume(id, stepId)) >> ST.set(ResumeRequested).liftCB

  private val component = ScalaComponent.builder[Props]("StepsControlButtons")
    .initialState(NoneRequested)
    .renderPS { ($, p, s) =>
      <.div(
        ^.cls := "ui icon buttons",
        p.instrument.observationOperations(p.step).map {
          case PauseObservation            =>
            Button(Button.Props(icon = Some(IconPause), color = Some("teal"), dataTooltip = Some("Pause the current exposure"), onClick = $.runState(handleObsPause(p.id, p.step.id)), disabled = !s.canPause || p.step.isObservePaused))
          case StopObservation             =>
            Button(Button.Props(icon = Some(IconStop), color = Some("orange"), dataTooltip = Some("Stop the current exposure early"), onClick = $.runState(handleStop(p.id, p.step.id)), disabled = !s.canStop))
          case AbortObservation            =>
            Button(Button.Props(icon = Some(IconTrash), color = Some("red"), dataTooltip = Some("Abort the current exposure"), onClick = $.runState(handleAbort(p.id, p.step.id)), disabled = !s.canAbort))
          case ResumeObservation           =>
            Button(Button.Props(icon = Some(IconPlay), color = Some("blue"), dataTooltip = Some("Resume the current exposure"), onClick = $.runState(handleObsResume(p.id, p.step.id)), disabled = !s.canResume || !p.step.isObservePaused))
          // Hamamatsu operations
          case PauseImmediatelyObservation =>
            Button(Button.Props(icon = Some(IconPause), color = Some("teal"), dataTooltip = Some("Pause the current exposure immediately")))
          case PauseGracefullyObservation  =>
            Button(Button.Props(icon = Some(IconPause), color = Some("teal"), basic = true, dataTooltip = Some("Pause the current exposure gracefully")))
          case StopImmediatelyObservation  =>
            Button(Button.Props(icon = Some(IconStop), color = Some("orange"), dataTooltip = Some("Stop the current exposure immediately")))
          case StopGracefullyObservation   =>
            Button(Button.Props(icon = Some(IconStop), color = Some("orange"), basic = true, dataTooltip = Some("Stop the current exposure gracefully")))
        }.toTagMod
      )
    }.componentWillReceiveProps { f =>
      f.runState(f.nextProps.step match {
        case s if s.isObservePaused =>
          ST.set(NoneRequested)
        case _ =>
          ST.nop
      })
    }.build

  def apply(id: SequenceId, instrument: Instrument, state: SequenceState, step: Step): Unmounted[Props, State, Unit] = component(Props(id, instrument, state, step))
}
