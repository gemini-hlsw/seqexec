// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ScalaComponent
import gem.Observation
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.operations.ObservationOperations._
import seqexec.model.operations._
import seqexec.web.client.actions.RequestAbort
import seqexec.web.client.actions.RequestObsPause
import seqexec.web.client.actions.RequestObsResume
import seqexec.web.client.actions.RequestStop
import seqexec.web.client.model.TabOperations
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.semanticui.elements.icon.Icon.IconPause
import seqexec.web.client.semanticui.elements.icon.Icon.IconPlay
import seqexec.web.client.semanticui.elements.icon.Icon.IconStop
import seqexec.web.client.semanticui.elements.icon.Icon.IconTrash
import seqexec.web.client.reusability._
import web.client.style._

/**
  * Contains the control buttons like stop/abort at the row level
  */
object StepsControlButtons {
  final case class Props(id:              Observation.Id,
                         instrument:      Instrument,
                         sequenceState:   SequenceState,
                         stepId:          Int,
                         isObservePaused: Boolean,
                         tabOperations:   TabOperations) {
    val requestInFlight = tabOperations.stepRequestInFlight
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  def requestStop(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestStop(id, stepId))

  def requestAbort(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestAbort(id, stepId))

  def requestObsPause(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestObsPause(id, stepId))

  def requestObsResume(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestObsResume(id, stepId))

  private val component = ScalaComponent
    .builder[Props]("StepsControlButtons")
    .render_P { p =>
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
                                 requestObsPause(p.id, p.stepId),
                               disabled = p.requestInFlight || p.isObservePaused))
              )
            case StopObservation =>
              Popup(
                Popup.Props("button", "Stop the current exposure early"),
                Button(
                  Button.Props(icon     = Some(IconStop),
                               color    = Some("orange"),
                               onClick  = requestStop(p.id, p.stepId),
                               disabled = p.requestInFlight))
              )
            case AbortObservation =>
              Popup(
                Popup.Props("button", "Abort the current exposure"),
                Button(
                  Button.Props(
                    icon     = Some(IconTrash),
                    color    = Some("red"),
                    onClick  = requestAbort(p.id, p.stepId),
                    disabled = p.requestInFlight))
              )
            case ResumeObservation =>
              Popup(
                Popup.Props("button", "Resume the current exposure"),
                Button(
                  Button.Props(icon  = Some(IconPlay),
                               color = Some("blue"),
                               onClick =
                                 requestObsResume(p.id, p.stepId),
                               disabled = p.requestInFlight || !p.isObservePaused))
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
    // .configure(Reusability.shouldComponentUpdate)
    // .componentWillReceiveProps { f =>
    //   val startedRunning = f.currentProps.sequenceState.isRunning && !f.currentProps.sequenceState.isRunning
    //   f.runState(if (f.nextProps.isObservePaused || startedRunning) {
    //     ST.set(NoneRequested)
    //   } else {
    //     ST.nop
    //   })
    // }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
