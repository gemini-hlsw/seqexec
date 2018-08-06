// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import diode.react.ModelProxy
import cats.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CallbackTo, ScalaComponent, CatsReact}
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import gem.Observation
import mouse.all._
import seqexec.model.SequenceState
import seqexec.web.client.circuit.{SeqexecCircuit, SequenceControlFocus, ControlModel}
import seqexec.web.client.actions.{RequestCancelPause, RequestPause, RequestSync, RequestRun}
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.components.sequence.SequenceStepsTableContainer
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.button.Button.LeftLabeled
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.semanticui.elements.icon.Icon
import seqexec.web.client.semanticui.elements.icon.Icon.{IconRefresh, IconPlay, IconPause, IconBan}
import web.client.style._

/**
  * Control buttons for the sequence
  */
object SequenceControl {
  final case class Props(p: ModelProxy[SequenceControlFocus])
  final case class State(runRequested: Boolean, pauseRequested: Boolean, syncRequested: Boolean, cancelPauseRequested: Boolean) {
    val canRun: Boolean = !runRequested && !pauseRequested && !syncRequested
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

  def requestRun(s: Observation.Id): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(SeqexecCircuit.dispatchCB(RequestRun(s))) >> ST.mod(_.requestRun).liftCB

  def requestSync(s: Observation.Id): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(SeqexecCircuit.dispatchCB(RequestSync(s))) >> ST.mod(_.requestSync).liftCB

  def requestPause(s: Observation.Id): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(SeqexecCircuit.dispatchCB(RequestPause(s))) >> ST.mod(_.requestPause).liftCB

  def requestCancelPause(s: Observation.Id): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(SeqexecCircuit.dispatchCB(RequestCancelPause(s))) >> ST.mod(_.requestCancelPause).liftCB

  private def controlButton(icon: Icon, color: String, onClick: Callback, disabled: Boolean, tooltip: String, text: String) =
    Popup(Popup.Props("button", tooltip),
      Button(
        Button.Props(
          icon = Some(icon),
          labeled = LeftLabeled,
          onClick = onClick,
          color = Some(color),
          disabled = disabled),
        text
      )
    )

  private def component = ScalaComponent.builder[Props]("SequencesDefaultToolbar")
    .initialState(State.Zero)
    .renderPS { ($, p, s) =>
      val SequenceControlFocus(isLogged, isConnected, control, syncInProgress) = p.p()
      val allowedToExecute = isLogged && isConnected
      val canSync = !syncInProgress && !s.syncRequested
      <.div(
        control.whenDefined { m =>
          val ControlModel(id, isPartiallyExecuted, nextStep, status, inConflict) = m
          val nextStepToRun = nextStep.getOrElse(0) + 1
          val runContinueTooltip = s"${isPartiallyExecuted.fold("Continue", "Run")} the sequence from the step $nextStepToRun"
          val runContinueButton = s"${isPartiallyExecuted.fold("Continue", "Run")} from step $nextStepToRun"
          List(
            // Sync button
            controlButton(IconRefresh, "purple", $.runState(requestSync(id)), (!allowedToExecute || !canSync) && !inConflict, "Sync sequence", "Sync")
              .when(status.isIdle || status.isError),
            // Run button
            controlButton(IconPlay, "blue", $.runState(requestRun(id)), (!allowedToExecute || !s.canRun) && !inConflict, runContinueTooltip, runContinueButton)
              .when(status.isIdle || status.isError),
            // Cancel pause button
            controlButton(IconBan, "brown", $.runState(requestCancelPause(id)), !allowedToExecute || !s.canCancelPause, "Cancel process to pause the sequence", "Cancel Pause")
              .when(status.userStopRequested),
            // Pause button
            controlButton(IconPause, "teal", $.runState(requestPause(id)), !allowedToExecute || !s.canPause, "Pause the sequence after the current step completes", "Pause")
              .when(status.isRunning && !status.userStopRequested),
            // Resume
            controlButton(IconPlay, "teal", $.runState(requestPause(id)), !allowedToExecute || !s.canResume, "Resume the sequence", s"Continue from step $nextStepToRun")
              .when(status === SequenceState.Stopped)
          ).toTagMod
        }
      )
    }.componentWillReceiveProps { f =>
      // Update state of run requested and sync requested depending on the run state
      Callback.when(!f.nextProps.p().syncInProgress && f.state.syncRequested)(f.modState(_.copy(syncRequested = false))) *>
      Callback.when(f.nextProps.p().control.map(_.status).exists(_.isRunning) && f.state.runRequested)(f.modState(_.copy(runRequested = false)))
    }.build

  def apply(p: ModelProxy[SequenceControlFocus]): Unmounted[Props, State, Unit] = component(Props(p))
}

/**
  * Toolbar for logged in users
  */
@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
object SequenceDefaultToolbar {
  private val component = ScalaComponent.builder[SequenceStepsTableContainer.Props]("SequenceDefaultToolbar")
    .stateless
    .render_P ( p =>
      <.div(
        ^.cls := "ui grid",
        <.div(
        ^.cls := "ui row",
          SeqexecStyles.shorterRow,
          <.div(
            ^.cls := "ui sixteen wide column",
            SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(p.p().obsId))(SequenceInfo.apply)
          )
        ),
        <.div(
          ^.cls := "ui row",
          SeqexecStyles.shorterRow,
          SeqexecStyles.lowerRow,
          <.div(
            ^.cls := "ui left floated column eight wide computer eight wide tablet only",
            SeqexecCircuit.connect(SeqexecCircuit.sequenceControlReader(p.p().obsId))(SequenceControl.apply)
          ),
          <.div(
            ^.cls := "ui right floated column eight wide computer eight wide tablet sixteen wide mobile",
            SeqexecStyles.observerField.when(p.p().isLogged),
            SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(p.p().obsId))(SequenceObserverField.apply)
          )
        )
      )
    ).build

  def apply(p: SequenceStepsTableContainer.Props): Unmounted[SequenceStepsTableContainer.Props, Unit, Unit] = component(p)
}
