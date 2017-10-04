// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence

import japgolly.scalajs.react.{Callback, CallbackTo, ScalaComponent, ScalazReact}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.ScalazReact._
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconPause, IconPlay, IconStop, IconTrash}
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.model.operations.ObservationOperations._
import edu.gemini.seqexec.model.operations._
import edu.gemini.seqexec.web.client.actions.{RequestAbort, RequestStop}
import edu.gemini.seqexec.web.client.circuit.SeqexecCircuit

/**
 * Contains the control buttons like stop/abort at the row level
 */
object StepsControlButtons {
  final case class Props(id: SequenceId, instrument: Instrument, sequenceState: SequenceState, step: Step)
  final case class State(stopRequested: Boolean, abortRequested: Boolean)

  private val ST = ReactS.Fix[State]

  def requestStop(id: SequenceId, stepId: Int): Callback =
    Callback(SeqexecCircuit.dispatch(RequestStop(id, stepId)))

  def requestAbort(id: SequenceId, stepId: Int): Callback =
    Callback(SeqexecCircuit.dispatch(RequestAbort(id, stepId)))

  def handleStop(id: SequenceId, stepId: Int): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(requestStop(id, stepId)) >> ST.mod(_.copy(stopRequested = true, abortRequested = true)).liftCB

  def handleAbort(id: SequenceId, stepId: Int): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(requestAbort(id, stepId)) >> ST.mod(_.copy(abortRequested = true, stopRequested = true)).liftCB

  private val component = ScalaComponent.builder[Props]("StepsControlButtons")
    .initialState(State(false, false))
    .renderPS { ($, p, s) =>
      <.div(
        ^.cls := "ui icon buttons",
        p.instrument.observationOperations.map {
          case PauseObservation            =>
            Button(Button.Props(icon = Some(IconPause), color = Some("teal"), dataTooltip = Some("Pause the current exposure")))
          case StopObservation             =>
            Button(Button.Props(icon = Some(IconStop), color = Some("orange"), dataTooltip = Some("Stop the current exposure early"), disabled = s.stopRequested, onClick = $.runState(handleStop(p.id, p.step.id))))
          case AbortObservation            =>
            Button(Button.Props(icon = Some(IconTrash), color = Some("red"), dataTooltip = Some("Abort the current exposure"), onClick = $.runState(handleAbort(p.id, p.step.id))))
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
        }.toTagMod
      )
    }.build

  def apply(id: SequenceId, instrument: Instrument, state: SequenceState, step: Step): Unmounted[Props, State, Unit] = component(Props(id, instrument, state, step))
}
