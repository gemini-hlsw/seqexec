// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.toolbars

import edu.gemini.seqexec.model.Model.{SequenceId, SequenceState}
import edu.gemini.seqexec.web.client.circuit.{SeqexecCircuit, SequenceControlFocus, ControlModel}
import edu.gemini.seqexec.web.client.actions.{RequestCancelPause, RequestPause, RequestSync, RequestRun}
import edu.gemini.seqexec.web.client.ModelOps._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.Size
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconRefresh, IconCheckmark, IconPlay, IconPause, IconBan}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CallbackTo, ScalaComponent, ScalazReact}
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import diode.react.ModelProxy

import scalaz.syntax.equal._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._

/**
  * Control buttons for the sequence
  */
object SequenceControl {
  final case class Props(p: ModelProxy[SequenceControlFocus])
  final case class State(runRequested: Boolean, pauseRequested: Boolean, syncRequested: Boolean, cancelPauseRequested: Boolean) {
    val canRun: Boolean = !runRequested && !pauseRequested && !syncRequested
    val canSync: Boolean = canRun
    val canPause: Boolean = !pauseRequested && !syncRequested
    val canCancelPause: Boolean = !pauseRequested && !syncRequested
    val canResume: Boolean = !pauseRequested && !syncRequested && !runRequested

    def requestRun: State = copy(runRequested = true, pauseRequested = false, syncRequested = false, cancelPauseRequested = false)
    def requestSync: State = copy(runRequested = false, pauseRequested = false, syncRequested = true, cancelPauseRequested = false)
    def requestPause: State = copy(runRequested = false, pauseRequested = true, syncRequested = false, cancelPauseRequested = false)
    def requestCancelPause: State = copy(runRequested = false, pauseRequested = false, syncRequested = false, cancelPauseRequested = true)
  }

  object State {
    val Zero: State = State(runRequested = false, pauseRequested = false, syncRequested = false, cancelPauseRequested = false)
  }

  private val ST = ReactS.Fix[State]

  def requestRun(s: SequenceId): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(Callback(SeqexecCircuit.dispatch(RequestRun(s)))) >> ST.mod(_.requestRun).liftCB

  def requestSync(s: SequenceId): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(Callback(SeqexecCircuit.dispatch(RequestSync(s)))) >> ST.mod(_.requestSync).liftCB

  def requestPause(s: SequenceId): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(Callback(SeqexecCircuit.dispatch(RequestPause(s)))) >> ST.mod(_.requestPause).liftCB

  def requestCancelPause(s: SequenceId): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(Callback(SeqexecCircuit.dispatch(RequestCancelPause(s)))) >> ST.mod(_.requestCancelPause).liftCB

  private def controlButton(icon: Icon, color: String, onClick: Callback, disabled: Boolean, tooltip: String, text: String) =
    Button(
      Button.Props(
        icon = Some(icon),
        labeled = true,
        onClick = onClick,
        color = Some(color),
        dataTooltip = Some(tooltip),
        disabled = disabled),
      text
    )

  private def component = ScalaComponent.builder[Props]("SequencesDefaultToolbar")
    .initialState(State.Zero)
    .renderPS { ($, p, s) =>
      val SequenceControlFocus(isLogged, isConnected, control) = p.p()
      val allowedToExecute = isLogged && isConnected
      <.div(
        control.whenDefined { m =>
          val ControlModel(id, isPartiallyExecuted, nextStep, status) = m
          val nextStepToRun = nextStep.getOrElse(0) + 1
          val runContinueTooltip = s"${isPartiallyExecuted ? "Continue" | "Run"} the sequence from the step $nextStepToRun"
          val runContinueButton = s"${isPartiallyExecuted ? "Continue" | "Run"} from step $nextStepToRun"
          List(
            Label(Label.Props("Sequence Complete", color = "green".some, icon = IconCheckmark.some, size = Size.Big)).when(status === SequenceState.Completed),
            // Sync button
            controlButton(IconRefresh, "purple", $.runState(requestSync(id)), !allowedToExecute || !s.canSync, "Sync sequence", "Sync")
              .when(status === SequenceState.Idle),
            // Run button
            controlButton(IconPlay, "blue", $.runState(requestRun(id)), !allowedToExecute || !s.canRun, runContinueTooltip, runContinueButton)
              .when(status === SequenceState.Idle || status.isError),
            // Cancel pause button
            controlButton(IconBan, "brown", $.runState(requestCancelPause(id)), !allowedToExecute || !s.canCancelPause, "Cancel process to pause the sequence", "Cancel Pause")
              .when(status === SequenceState.Pausing),
            // Pause button
            controlButton(IconPause, "teal", $.runState(requestPause(id)), !allowedToExecute || !s.canPause, "Pause the sequence after the current step completes", "Pause")
              .when(status === SequenceState.Running),
            // Resume
            controlButton(IconPlay, "teal", $.runState(requestPause(id)), !allowedToExecute || !s.canResume, "Resume the sequence", s"Continue from step $nextStepToRun")
              .when(status === SequenceState.Paused)
          ).toTagMod
        }
      )
    }.componentWillReceiveProps { f =>
      // Update state of run requested depending on the run state
      Callback.when(f.nextProps.p().control.map(_.status).contains(SequenceState.Running) && f.state.runRequested)(f.modState(_.copy(runRequested = false)))
    }.build

  def apply(p: ModelProxy[SequenceControlFocus]): Unmounted[Props, State, Unit] = component(Props(p))
}
