// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import diode.react.ModelProxy
import diode.react.ReactConnectProxy
import cats.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.CatsReact
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import gem.Observation
import mouse.all._
import seqexec.model.SequenceState
import seqexec.web.client.circuit._
import seqexec.web.client.actions.RequestCancelPause
import seqexec.web.client.actions.RequestPause
import seqexec.web.client.actions.RequestSync
import seqexec.web.client.actions.RequestRun
import seqexec.web.client.model.RunOperation
import seqexec.web.client.model.PauseOperation
import seqexec.web.client.model.SyncOperation
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.button.Button.LeftLabeled
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.semanticui.elements.icon.Icon
import seqexec.web.client.semanticui.elements.icon.Icon.IconRefresh
import seqexec.web.client.semanticui.elements.icon.Icon.IconPlay
import seqexec.web.client.semanticui.elements.icon.Icon.IconPause
import seqexec.web.client.semanticui.elements.icon.Icon.IconBan
import seqexec.web.client.reusability._
import web.client.style._

/**
  * Control buttons for the sequence
  */
object SequenceControl {
  type Backend = RenderScope[Props, State, Unit]

  final case class Props(p: SequenceControlFocus) {
    val runRequested: RunOperation =
      p.control
        .map(_.tabOperations.runRequested)
        .getOrElse(RunOperation.RunIdle)

    val syncRequested: SyncOperation =
      p.control
        .map(_.tabOperations.syncRequested)
        .getOrElse(SyncOperation.SyncIdle)

    val pauseRequested: PauseOperation =
      p.control
        .map(_.tabOperations.pauseRequested)
        .getOrElse(PauseOperation.PauseIdle)

    val isRunning: Boolean = p.control.map(_.status).exists(_.isRunning)
  }

  final case class State(runRequested:         RunOperation,
                         pauseRequested:       PauseOperation,
                         syncRequested:        SyncOperation,
                         cancelPauseRequested: Boolean) {
    val syncIdle: Boolean  = syncRequested === SyncOperation.SyncIdle
    val pauseIdle: Boolean = pauseRequested === PauseOperation.PauseIdle
    val canRun: Boolean =
      (runRequested === RunOperation.RunIdle) && pauseIdle && syncIdle
    val canPause: Boolean       = pauseIdle && syncIdle
    val canCancelPause: Boolean = pauseIdle && syncIdle
    val canResume: Boolean =
      pauseIdle && syncIdle && (runRequested === RunOperation.RunIdle)

    def requestRun: State =
      copy(runRequested         = RunOperation.RunInFlight,
           pauseRequested       = PauseOperation.PauseIdle,
           syncRequested        = SyncOperation.SyncIdle,
           cancelPauseRequested = false)

    def requestSync: State =
      copy(runRequested         = RunOperation.RunIdle,
           pauseRequested       = PauseOperation.PauseIdle,
           syncRequested        = SyncOperation.SyncInFlight,
           cancelPauseRequested = false)

    def requestPause: State =
      copy(runRequested         = RunOperation.RunIdle,
           pauseRequested       = PauseOperation.PauseInFlight,
           syncRequested        = SyncOperation.SyncIdle,
           cancelPauseRequested = false)

    def requestCancelPause: State =
      copy(runRequested         = RunOperation.RunIdle,
           pauseRequested       = PauseOperation.PauseIdle,
           syncRequested        = SyncOperation.SyncIdle,
           cancelPauseRequested = true)
  }

  object State {
    val Zero: State = State(runRequested = RunOperation.RunIdle,
                            pauseRequested       = PauseOperation.PauseIdle,
                            syncRequested        = SyncOperation.SyncIdle,
                            cancelPauseRequested = false)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  implicit val stateReuse: Reusability[State] = Reusability.derive[State]

  private val ST = ReactS.Fix[State]

  def requestRun(s: Observation.Id): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(SeqexecCircuit.dispatchCB(RequestRun(s))) >>
      ST.mod(_.requestRun).liftCB

  def requestSync(s: Observation.Id): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(SeqexecCircuit.dispatchCB(RequestSync(s))) >>
      ST.mod(_.requestSync).liftCB

  def requestPause(s: Observation.Id): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(SeqexecCircuit.dispatchCB(RequestPause(s))) >>
      ST.mod(_.requestPause).liftCB

  def requestCancelPause(s: Observation.Id): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(SeqexecCircuit.dispatchCB(RequestCancelPause(s))) >>
      ST.mod(_.requestCancelPause).liftCB

  private def controlButton(icon:     Icon,
                            color:    String,
                            onClick:  Callback,
                            disabled: Boolean,
                            tooltip:  String,
                            text:     String) =
    Popup(Popup.Props("button", tooltip),
          Button(
            Button.Props(icon     = Some(icon),
                         labeled  = LeftLabeled,
                         onClick  = onClick,
                         color    = Some(color),
                         disabled = disabled),
            text
          ))

  def stateFromProps(p: Props): State =
    State.Zero.copy(runRequested = p.runRequested)

  private def syncButton(b: Backend, id: Observation.Id, canOperate: Boolean, canSync: Boolean) =
    controlButton(icon     = IconRefresh,
                  color    = "purple",
                  onClick  = b.runState(requestSync(id)),
                  disabled = (!canOperate || !canSync),
                  tooltip  = "Sync sequence",
                  text     = "Sync")

  private def runButton(b: Backend, id: Observation.Id, isPartiallyExecuted: Boolean, nextStepToRun: Int, canOperate: Boolean, canRun: Boolean) = {
    val runContinueTooltip =
      s"${isPartiallyExecuted.fold("Continue", "Run")} the sequence from the step $nextStepToRun"
    val runContinueButton =
      s"${isPartiallyExecuted.fold("Continue", "Run")} from step $nextStepToRun"
    controlButton(icon     = IconPlay,
                  color    = "blue",
                  onClick  = b.runState(requestRun(id)),
                  disabled = (!canOperate || !canRun),
                  tooltip  = runContinueTooltip,
                  text     = runContinueButton)
  }

  private def cancelPauseButton(b: Backend, id: Observation.Id, canOperate: Boolean, canCancelPause: Boolean) =
    controlButton(
      icon     = IconBan,
      color    = "brown",
      onClick  = b.runState(requestCancelPause(id)),
      disabled = !canOperate || !canCancelPause,
      tooltip  = "Cancel process to pause the sequence",
      text     = "Cancel Pause"
    )

  private def pauseButton(b: Backend, id: Observation.Id, canOperate: Boolean, canPause: Boolean) =
    controlButton(
      icon     = IconPause,
      color    = "teal",
      onClick  = b.runState(requestPause(id)),
      disabled = !canOperate || !canPause,
      tooltip  = "Pause the sequence after the current step completes",
      text     = "Pause"
    )

  private def resumeButton(b: Backend, id: Observation.Id, nextStepToRun: Int, canOperate: Boolean, canResume: Boolean) =
    controlButton(
      icon     = IconPlay,
      color    = "teal",
      onClick  = b.runState(requestPause(id)),
      disabled = !canOperate || !canResume,
      tooltip  = "Resume the sequence",
      text     = s"Continue from step $nextStepToRun"
    )

  private def component =
    ScalaComponent
      .builder[Props]("SequencesDefaultToolbar")
      .initialStateFromProps(stateFromProps)
      .renderPS { (b, p, s) =>
        val SequenceControlFocus(canOperate, control) = p.p
        <.div(
          SeqexecStyles.controlButtons,
          control.whenDefined { m =>
            val ControlModel(id, partial, nextStep, status, to) = m

            val syncInProgress = to.syncRequested === SyncOperation.SyncInFlight
            val canSync        = !syncInProgress && s.syncIdle
            val nextStepToRun  = nextStep.getOrElse(0) + 1
            List(
              // Sync button
              syncButton(b, id, canOperate, canSync)
                .when(status.isIdle || status.isError),
              // Run button
              runButton(b, id, partial, nextStepToRun, canOperate, s.canRun)
                .when(status.isIdle || status.isError),
              // Cancel pause button
              cancelPauseButton(b, id, canOperate, s.canCancelPause)
                .when(status.userStopRequested),
              // Pause button
              pauseButton(b, id, canOperate, s.canPause)
                .when(status.isRunning && !status.userStopRequested),
              // Resume
              resumeButton(b, id, nextStepToRun, canOperate, s.canResume)
                .when(status === SequenceState.Stopped)
            ).toTagMod
          }
        )
      }
      .componentWillReceiveProps { f =>
        // Update state of run requested and sync requested depending on the run state
        Callback.when(
          !(f.nextProps.syncRequested === SyncOperation.SyncInFlight) && !f.state.syncIdle)(
          f.modState(_.copy(syncRequested = SyncOperation.SyncIdle))) *>
          Callback.when(
            ((f.nextProps.runRequested === RunOperation.RunIdle) || f.nextProps.isRunning) && f.state.runRequested === RunOperation.RunInFlight)(
            f.modState(_.copy(runRequested = RunOperation.RunIdle))) *>
          Callback.when(
            !(f.nextProps.pauseRequested === PauseOperation.PauseInFlight) || !f.state.pauseIdle)(
            f.modState(_.copy(pauseRequested = PauseOperation.PauseIdle)))
      }
      .build

  def apply(p: ModelProxy[SequenceControlFocus]): Unmounted[Props, State, Unit] =
    component(Props(p()))
}

/**
  * Toolbar for logged in users
  */
object SequenceDefaultToolbar {
  final case class Props(id: Observation.Id) {
    val observerReader: ReactConnectProxy[SequenceInfoFocus] =
      SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(id))
    val controlReader: ReactConnectProxy[SequenceControlFocus] =
      SeqexecCircuit.connect(SeqexecCircuit.sequenceControlReader(id))
  }

  private val component = ScalaComponent
    .builder[Props]("SequenceDefaultToolbar")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui grid",
        <.div(
          ^.cls := "two column row",
          SeqexecStyles.shorterRow,
          <.div(
            ^.cls := "ui left floated column eight wide computer eight wide tablet only",
            p.controlReader(SequenceControl.apply)
          ),
          <.div(
            ^.cls := "ui right floated column",
            SeqexecStyles.infoOnControl,
            p.observerReader(p => SequenceInfo(SequenceInfo.Props(p)))
          )
        )
    ))
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
