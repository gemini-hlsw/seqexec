// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.CatsReact
import gem.Observation
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.operations.ObservationOperations._
import seqexec.model.operations._
import seqexec.web.client.actions.RequestAbort
import seqexec.web.client.actions.RequestObsPause
import seqexec.web.client.actions.RequestObsResume
import seqexec.web.client.actions.RequestStop
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.semanticui.elements.icon.Icon.IconPause
import seqexec.web.client.semanticui.elements.icon.Icon.IconPlay
import seqexec.web.client.semanticui.elements.icon.Icon.IconStop
import seqexec.web.client.semanticui.elements.icon.Icon.IconTrash
import web.client.style._

/**
  * Contains the control buttons like stop/abort at the row level
  */
object StepsControlButtons {
  final case class Props(id:              Observation.Id,
                         instrument:      Instrument,
                         sequenceState:   SequenceState,
                         stepId:          Int,
                         isObservePaused: Boolean)

  final case class State(stopRequested:   Boolean,
                         abortRequested:  Boolean,
                         pauseRequested:  Boolean,
                         resumeRequested: Boolean) {

    val canPause: Boolean  = !stopRequested && !abortRequested
    val canAbort: Boolean  = !stopRequested && !pauseRequested
    val canStop: Boolean   = !abortRequested && !pauseRequested
    val canResume: Boolean = canPause
  }

  val StopRequested: State = State(stopRequested = true,
                                   abortRequested  = false,
                                   pauseRequested  = false,
                                   resumeRequested = false)
  val AbortRequested: State = State(stopRequested = false,
                                    abortRequested  = true,
                                    pauseRequested  = false,
                                    resumeRequested = false)
  val PauseRequested: State = State(stopRequested = false,
                                    abortRequested  = false,
                                    pauseRequested  = true,
                                    resumeRequested = false)
  val ResumeRequested: State = State(stopRequested = false,
                                     abortRequested  = false,
                                     pauseRequested  = false,
                                     resumeRequested = true)
  val NoneRequested: State = State(stopRequested = false,
                                   abortRequested  = false,
                                   pauseRequested  = false,
                                   resumeRequested = false)

  private val ST = ReactS.Fix[State]

  def requestStop(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestStop(id, stepId))

  def requestAbort(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestAbort(id, stepId))

  def requestObsPause(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestObsPause(id, stepId))

  def requestObsResume(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestObsResume(id, stepId))

  def handleStop(id:     Observation.Id,
                 stepId: Int): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(requestStop(id, stepId)) >> ST.set(StopRequested).liftCB

  def handleAbort(id:     Observation.Id,
                  stepId: Int): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(requestAbort(id, stepId)) >> ST.set(AbortRequested).liftCB

  def handleObsPause(id:     Observation.Id,
                     stepId: Int): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(requestObsPause(id, stepId)) >> ST.set(PauseRequested).liftCB

  def handleObsResume(id:     Observation.Id,
                      stepId: Int): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(requestObsResume(id, stepId)) >> ST.set(ResumeRequested).liftCB

  private val component = ScalaComponent
    .builder[Props]("StepsControlButtons")
    .initialState(NoneRequested)
    .renderPS { ($, p, s) =>
      <.div(
        ^.cls := "ui icon buttons",
        SeqexecStyles.notInMobile,
        p.instrument
          .observationOperations(p.isObservePaused)
          .map {
            case PauseObservation =>
              Popup(
                Popup.Props("button", "Pause the current exposure"),
                Button(
                  Button.Props(icon  = Some(IconPause),
                               color = Some("teal"),
                               onClick =
                                 $.runState(handleObsPause(p.id, p.stepId)),
                               disabled = !s.canPause || p.isObservePaused))
              )
            case StopObservation =>
              Popup(
                Popup.Props("button", "Stop the current exposure early"),
                Button(
                  Button.Props(icon     = Some(IconStop),
                               color    = Some("orange"),
                               onClick  = $.runState(handleStop(p.id, p.stepId)),
                               disabled = !s.canStop))
              )
            case AbortObservation =>
              Popup(
                Popup.Props("button", "Abort the current exposure"),
                Button(
                  Button.Props(
                    icon     = Some(IconTrash),
                    color    = Some("red"),
                    onClick  = $.runState(handleAbort(p.id, p.stepId)),
                    disabled = !s.canAbort))
              )
            case ResumeObservation =>
              Popup(
                Popup.Props("button", "Resume the current exposure"),
                Button(
                  Button.Props(icon  = Some(IconPlay),
                               color = Some("blue"),
                               onClick =
                                 $.runState(handleObsResume(p.id, p.stepId)),
                               disabled = !s.canResume || !p.isObservePaused))
              )
            // Hamamatsu operations
            case PauseImmediatelyObservation =>
              Popup(
                Popup.Props("button", "Pause the current exposure immediately"),
                Button(
                  Button.Props(icon = Some(IconPause), color = Some("teal"))))
            case PauseGracefullyObservation =>
              Popup(Popup.Props("button",
                                "Pause the current exposure gracefully"),
                    Button(
                      Button.Props(icon  = Some(IconPause),
                                   color = Some("teal"),
                                   basic = true)))
            case StopImmediatelyObservation =>
              Popup(
                Popup.Props("button", "Stop the current exposure immediately"),
                Button(
                  Button.Props(icon = Some(IconStop), color = Some("orange"))))
            case StopGracefullyObservation =>
              Popup(Popup.Props("button",
                                "Stop the current exposure gracefully"),
                    Button(
                      Button.Props(icon  = Some(IconStop),
                                   color = Some("orange"),
                                   basic = true)))
          }
          .toTagMod
      )
    }
    .componentWillReceiveProps { f =>
      f.runState(if (f.nextProps.isObservePaused) {
        ST.set(NoneRequested)
      } else {
        ST.nop
      })
    }
    .build

  def apply(p: Props): Unmounted[Props, State, Unit] = component(p)
}
