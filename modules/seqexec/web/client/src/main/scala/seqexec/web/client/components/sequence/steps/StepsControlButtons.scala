// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.syntax.all._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.colors._
import react.semanticui.elements.button.Button
import react.semanticui.elements.icon._
import react.semanticui.modules.popup.Popup
import react.semanticui.modules.popup.PopupPosition
import seqexec.model.Observation
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.operations.Operations._
import seqexec.model.operations._
import seqexec.web.client.actions._
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.icons._
import seqexec.web.client.model.TabOperations
import seqexec.web.client.reusability._

/**
  * Contains a set of control buttons like stop/abort
  */
final case class ControlButtons(
  obsId:               Observation.Id,
  operations:          List[Operations[_]],
  sequenceState:       SequenceState,
  stepId:              Int,
  isObservePaused:     Boolean,
  tabOperations:       TabOperations,
  nsPendingObserveCmd: Option[NodAndShuffleStep.PendingObserveCmd] = None
) extends ReactProps[ControlButtons](ControlButtons.component) {

  val requestInFlight = tabOperations.stepRequestInFlight

  protected[steps] val connect =
    SeqexecCircuit.connect(SeqexecCircuit.obsProgressReader[Progress](obsId, stepId))
}

object ControlButtons {
  type Props = ControlButtons

  implicit val operationsReuse: Reusability[Operations[_]] = Reusability.derive[Operations[_]]
  implicit val propsReuse: Reusability[Props]              = Reusability.derive[Props]

  private def requestStop(obsId: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestStop(obsId, stepId))

  private def requestGracefulStop(obsId: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestGracefulStop(obsId, stepId))

  private def requestAbort(obsId: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestAbort(obsId, stepId))

  private def requestObsPause(obsId: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestObsPause(obsId, stepId))

  private def requestGracefulObsPause(obsId: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestGracefulObsPause(obsId, stepId))

  private def requestObsResume(obsId: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestObsResume(obsId, stepId))

  private def requestedIcon(icon: Icon): IconGroup =
    IconGroup(
      icon(^.key := "main"),
      IconCircleNotched.copy(loading = true, color = Yellow)(^.key := "requested")
    )

  protected val component = ScalaComponent
    .builder[Props]("ControlButtons")
    .render_P { p =>
      val pauseGracefullyIcon: VdomNode =
        p.nsPendingObserveCmd
          .collect {
            case NodAndShuffleStep.PauseGracefully => requestedIcon(IconPause): VdomNode
          }
          .getOrElse(IconPause)

      val stopGracefullyIcon: VdomNode =
        p.nsPendingObserveCmd
          .collect {
            case NodAndShuffleStep.StopGracefully => requestedIcon(IconStop): VdomNode
          }
          .getOrElse(IconStop)

      p.connect { proxy =>
        val isReadingOut = proxy().exists(_.stage === ObserveStage.ReadingOut)

        <.div(
          ^.cls := "ui icon buttons",
          SeqexecStyles.notInMobile,
          p.operations.map {
            case PauseObservation =>
              Popup(
                position = PopupPosition.TopRight,
                trigger = Button(
                  icon     = true,
                  color    = Teal,
                  onClick  = requestObsPause(p.obsId, p.stepId),
                  disabled = p.requestInFlight || p.isObservePaused || isReadingOut
                )(IconPause)
              )("Pause the current exposure")
            case StopObservation =>
              Popup(
                position = PopupPosition.TopRight,
                trigger = Button(
                  icon     = true,
                  color    = Orange,
                  onClick  = requestStop(p.obsId, p.stepId),
                  disabled = p.requestInFlight || isReadingOut
                )(IconStop)
              )("Stop the current exposure early")
            case AbortObservation =>
              Popup(
                position = PopupPosition.TopRight,
                trigger = Button(
                  icon     = true,
                  color    = Red,
                  onClick  = requestAbort(p.obsId, p.stepId),
                  disabled = p.requestInFlight || isReadingOut
                )(IconTrash)
              )("Abort the current exposure")
            case ResumeObservation =>
              Popup(
                position = PopupPosition.TopRight,
                trigger = Button(
                  icon     = true,
                  color    = Blue,
                  onClick  = requestObsResume(p.obsId, p.stepId),
                  disabled = p.requestInFlight || !p.isObservePaused || isReadingOut
                )(IconPlay)
              )("Resume the current exposure")
            // N&S operations
            case PauseImmediatelyObservation =>
              Popup(
                position = PopupPosition.TopRight,
                trigger = Button(
                  icon     = true,
                  color    = Teal,
                  basic    = true,
                  onClick  = requestObsPause(p.obsId, p.stepId),
                  disabled = p.requestInFlight || p.isObservePaused || isReadingOut
                )(IconPause)
              )("Pause the current exposure immediately")
            case PauseGracefullyObservation =>
              Popup(
                position = PopupPosition.TopRight,
                trigger = Button(
                  icon    = true,
                  color   = Teal,
                  onClick = requestGracefulObsPause(p.obsId, p.stepId),
                  disabled =
                    p.requestInFlight || p.isObservePaused || p.nsPendingObserveCmd.isDefined || isReadingOut
                )(pauseGracefullyIcon)
              )("Pause the current exposure at the end of the cycle")
            case StopImmediatelyObservation =>
              Popup(
                position = PopupPosition.TopRight,
                trigger = Button(
                  icon     = true,
                  color    = Orange,
                  basic    = true,
                  onClick  = requestStop(p.obsId, p.stepId),
                  disabled = p.requestInFlight || isReadingOut
                )(IconStop)
              )("Stop the current exposure immediately")
            case StopGracefullyObservation =>
              Popup(
                position = PopupPosition.TopRight,
                trigger = Button(
                  icon    = true,
                  color   = Orange,
                  onClick = requestGracefulStop(p.obsId, p.stepId),
                  disabled =
                    p.requestInFlight || p.isObservePaused || p.nsPendingObserveCmd.isDefined || isReadingOut
                )(stopGracefullyIcon)
              )("Stop the current exposure at the end of the cycle")
          }.toTagMod
        )
      }
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}

/**
  * Contains the control buttons like stop/abort at the row level
  */
final case class StepsControlButtons(
  obsId:           Observation.Id,
  instrument:      Instrument,
  sequenceState:   SequenceState,
  stepId:          Int,
  isObservePaused: Boolean,
  isMultiLevel:    Boolean,
  tabOperations:   TabOperations
) extends ReactProps[StepsControlButtons](StepsControlButtons.component) {

  val requestInFlight = tabOperations.stepRequestInFlight
}

object StepsControlButtons {
  type Props = StepsControlButtons

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
    .builder[Props]("StepsControlButtons")
    .render_P { p =>
      ControlButtons(
        p.obsId,
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
