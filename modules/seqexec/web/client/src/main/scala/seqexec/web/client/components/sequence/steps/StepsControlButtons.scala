// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ScalaComponent
import gem.Observation
import react.common._
import react.common.implicits._
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.operations.Operations._
import seqexec.model.operations._
import seqexec.web.client.actions.{RequestAbort, RequestGracefulObsPause, RequestGracefulStop, RequestObsPause, RequestObsResume, RequestStop}
import seqexec.web.client.model.TabOperations
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.semanticui.elements.icon.Icon.IconGroup
import seqexec.web.client.semanticui.elements.icon.Icon.IconPause
import seqexec.web.client.semanticui.elements.icon.Icon.IconPlay
import seqexec.web.client.semanticui.elements.icon.Icon.IconStop
import seqexec.web.client.semanticui.elements.icon.Icon.IconTrash
import seqexec.web.client.reusability._
import seqexec.web.client.semanticui.elements.icon.Icon

/**
  * Contains a set of control buttons like stop/abort
  */
final case class ControlButtons(
  id:                   Observation.Id,
  operations:           List[Operations[_]],
  sequenceState:        SequenceState,
  stepId:               Int,
  isObservePaused:      Boolean,
  tabOperations:        TabOperations,
  nsPendingObserveCmd:  Option[NodAndShuffleStep.PendingObserveCmd] = None
) extends ReactProps {
  @inline def render: VdomElement = ControlButtons.component(this)

  val requestInFlight = tabOperations.stepRequestInFlight
}

object ControlButtons {
  type Props = ControlButtons

  implicit val operationsReuse: Reusability[Operations[_]] = Reusability.derive[Operations[_]]
  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private def requestStop(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestStop(id, stepId))

  private def requestGracefulStop(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestGracefulStop(id, stepId))

  private def requestAbort(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestAbort(id, stepId))

  private def requestObsPause(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestObsPause(id, stepId))

  private def requestGracefulObsPause(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestGracefulObsPause(id, stepId))

  private def requestObsResume(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestObsResume(id, stepId))

  private def requestedIcon(icon: Icon): Icon =
    IconGroup(
      icon.copyIcon(key = "main"),
      Icon.IconCircleNotched.copyIcon(key = "requested", loading = true, color = Some("yellow"))
    )

  protected val component = ScalaComponent
    .builder[Props]("ControlButtons")
    .render_P { p =>
      val pauseGracefullyIcon: Icon =
        p.nsPendingObserveCmd.collect{
          case NodAndShuffleStep.PauseGracefully => requestedIcon(IconPause)
        }.getOrElse(IconPause)

      val stopGracefullyIcon: Icon =
        p.nsPendingObserveCmd.collect{
          case NodAndShuffleStep.StopGracefully => requestedIcon(IconStop)
        }.getOrElse(IconStop)

      <.div(
        ^.cls := "ui icon buttons",
        SeqexecStyles.notInMobile,
        p.operations
         .map {
           case PauseObservation =>
             Popup("button", "Pause the current exposure")(
               Button(
                 icon     = Some(IconPause),
                 color    = Some("teal"),
                 onClick  = requestObsPause(p.id, p.stepId),
                 disabled = p.requestInFlight || p.isObservePaused
               )
             )
           case StopObservation =>
             Popup("button", "Stop the current exposure early")(
               Button(
                 icon     = Some(IconStop),
                 color    = Some("orange"),
                 onClick  = requestStop(p.id, p.stepId),
                 disabled = p.requestInFlight
               )
             )
           case AbortObservation =>
             Popup("button", "Abort the current exposure")(
               Button(
                 icon     = Some(IconTrash),
                 color    = Some("red"),
                 onClick  = requestAbort(p.id, p.stepId),
                 disabled = p.requestInFlight
               )
             )
           case ResumeObservation =>
             Popup("button", "Resume the current exposure")(
               Button(
                 icon     = Some(IconPlay),
                 color    = Some("blue"),
                 onClick  = requestObsResume(p.id, p.stepId),
                 disabled = p.requestInFlight || !p.isObservePaused
               )
             )
           // N&S operations
           case PauseImmediatelyObservation =>
             Popup("button", "Pause the current exposure immediately")(
               Button(
                 icon     = Some(IconPause),
                 color    = Some("teal"),
                 basic    = true,
                 onClick  = requestObsPause(p.id, p.stepId),
                 disabled = p.requestInFlight || p.isObservePaused
               )
             )
           case PauseGracefullyObservation =>
             Popup("button", "Pause the current exposure at the end of the cycle")(
               Button(
                 icon     = Some(pauseGracefullyIcon),
                 color    = Some("teal"),
                 onClick  = requestGracefulObsPause(p.id, p.stepId),
                 disabled = p.requestInFlight || p.isObservePaused || p.nsPendingObserveCmd.isDefined
               )
             )
           case StopImmediatelyObservation =>
             Popup("button", "Stop the current exposure immediately")(
               Button(
                 icon     = Some(IconStop),
                 color    = Some("orange"),
                 basic    = true,
                 onClick  = requestStop(p.id, p.stepId),
                 disabled = p.requestInFlight
               )
             )
           case StopGracefullyObservation =>
             Popup("button", "Stop the current exposure at the end of the cycle")(
               Button(
                 icon     = Some(stopGracefullyIcon),
                 color    = Some("orange"),
                 onClick  = requestGracefulStop(p.id, p.stepId),
                 disabled = p.requestInFlight || p.isObservePaused || p.nsPendingObserveCmd.isDefined
               )
             )
         }
         .toTagMod
        )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}


/**
  * Contains the control buttons like stop/abort at the row level
  */
final case class StepsControlButtons(
  id:              Observation.Id,
  instrument:      Instrument,
  sequenceState:   SequenceState,
  stepId:          Int,
  isObservePaused: Boolean,
  isMultiLevel:    Boolean,
  tabOperations:   TabOperations
) extends ReactProps {
  @inline def render: VdomElement = StepsControlButtons.component(this)

  val requestInFlight = tabOperations.stepRequestInFlight
}

object StepsControlButtons {
  type Props = StepsControlButtons

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
    .builder[Props]("StepsControlButtons")
    .render_P { p =>
      ControlButtons(
        p.id,
        p.instrument.operations[OperationLevel.Observation](p.isObservePaused, p.isMultiLevel),
        p.sequenceState,
        p.stepId,
        p.isObservePaused,
        p.tabOperations
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
