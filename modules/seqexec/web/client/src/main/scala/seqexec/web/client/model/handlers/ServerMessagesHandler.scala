// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import diode.{Action, ActionHandler, ActionResult, Effect, ModelRW, NoAction}
import seqexec.model.enum.{ ActionStatus }
import seqexec.model.{ Observer, SequencesQueue, SequenceView, StepState, SequenceState }
import seqexec.model.events._
import seqexec.web.client.lenses.{sequenceStepT, sequenceViewT}
import seqexec.web.client.ModelOps._
import seqexec.web.client.actions._
import seqexec.web.client.circuit._
import seqexec.web.client.services.{Audio, SeqexecWebClient}
import seqexec.web.client.services.WebpackResources._
import cats.implicits._

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
  * Handles messages received over the WS channel
  */
class ServerMessagesHandler[M](modelRW: ModelRW[M, WebSocketsFocus]) extends ActionHandler(modelRW) with Handlers {
  // Global references to audio files
  private val SequencePausedAudio = new Audio(SequencePausedResource.resource)
  private val ExposurePausedAudio = new Audio(ExposurePausedResource.resource)
  private val SequenceErrorAudio = new Audio(SequenceErrorResource.resource)
  private val SequenceCompleteAudio = new Audio(SequenceCompleteResource.resource)
  private val StepBeepAudio = new Audio(BeepResource.resource)

  // It is legal do put sequences of the other sites on the queue
  // but we don't know how to display them, so let's filter them out
  private def filterSequences(sequences: SequencesQueue[SequenceView]): SequencesQueue[SequenceView] =
    sequences.copy(queue = sequences.queue.filter {
      case SequenceView(_, metadata, _, _, _) => value.site.map(_.instruments.toList.contains(metadata.instrument)).getOrElse(false)
    })

  val soundCheck: PartialFunction[Any, ActionResult[M]] = {
    case RequestSoundEcho =>
      val soundEffect = Effect(Future(SequenceCompleteAudio.play()).map(_ => NoAction))
      effectOnly(soundEffect)
  }

  val logMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(l: ServerLogMessage) =>
      effectOnly(Effect(Future(AppendToLog(l))))
  }

  val connectionOpenMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(ConnectionOpenEvent(u, c)) =>
      // After connected to the Websocket request a refresh
      val refreshRequest = Effect(SeqexecWebClient.refresh(c).map(_ => NoAction))
      updated(value.copy(user = u, defaultObserver = u.map(m => Observer(m.displayName)).getOrElse(value.defaultObserver), clientId = Option(c)), refreshRequest)
  }

  val stepCompletedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(e @ StepExecuted(obsId, sv)) =>
    val curStep =
      for {
          obs      <- sequenceViewT.find(_.id === obsId)(e)
          curSIdx  <- obs.runningStep.map(_.last)
          curStep  <- sequenceStepT.find(_.id === curSIdx)(obs)
          if curStep.observeStatus === ActionStatus.Pending && curStep.status === StepState.Running
          if curStep.configStatus.map(_._2).forall(_ === ActionStatus.Pending)
        } yield curStep

      val audioEffect = curStep.fold(VoidEffect)(_ => Effect(Future(StepBeepAudio.play()).map(_ => NoAction)))
      updated(value.copy(sequences = filterSequences(sv)), audioEffect)
  }

  val sequenceCompletedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequenceCompleted(sv)) =>
      // Play audio when the sequence completes
      val audioEffect = Effect(Future(SequenceCompleteAudio.play()).map(_ => NoAction))
      val rememberCompleted = Effect(Future(sv.queue.find(_.status == SequenceState.Completed).fold(NoAction: Action)(RememberCompleted.apply)))
      updated(value.copy(sequences = filterSequences(sv)), audioEffect + rememberCompleted)
  }

  val sequenceOnErrorMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequenceError(_, sv)) =>
      // Play audio when the sequence gets into an error state
      val audioEffect = Effect(Future(SequenceErrorAudio.play()).map(_ => NoAction))
      updated(value.copy(sequences = filterSequences(sv)), audioEffect)
  }

  val sequencePausedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequencePaused(_, sv)) =>
      // Play audio when the sequence gets paused
      val audioEffect = Effect(Future(SequencePausedAudio.play()).map(_ => NoAction))
      updated(value.copy(sequences = filterSequences(sv)), audioEffect)
  }

  val exposurePausedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(ExposurePaused(_, sv)) =>
      // Play audio when the sequence gets paused
      val audioEffect = Effect(Future(ExposurePausedAudio.play()).map(_ => NoAction))
      updated(value.copy(sequences = filterSequences(sv)), audioEffect)
  }

  val observerUpdatedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(s: ObserverUpdated) =>
      updated(value.copy(sequences = filterSequences(s.view)))
  }

  val actionStoppedRequestMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(ActionStopRequested(svs)) =>
      updated(value.copy(sequences = filterSequences(svs)))
  }

  val resourceBusyMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(ResourcesBusy(id, _, _)) =>
      val setConflictE = Effect(Future(SequenceInConflict(id)))
      val openBoxE = Effect(Future(OpenResourcesBox))
      effectOnly(setConflictE >> openBoxE)
  }

  val sequenceLoadedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequenceLoaded(_, view)) =>
      updated(value.copy(sequences = filterSequences(view)))
  }

  val sequenceUnloadedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequenceUnloaded(_, view)) =>
      updated(value.copy(sequences = filterSequences(view)))
  }

  val modelUpdateMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(s: SeqexecModelUpdate) =>
      updated(value.copy(sequences = filterSequences(s.view)))
  }

  val defaultMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(_) =>
      // Ignore unknown events
      noChange
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(soundCheck,
      logMessage,
      stepCompletedMessage,
      connectionOpenMessage,
      sequenceCompletedMessage,
      sequenceOnErrorMessage,
      sequencePausedMessage,
      exposurePausedMessage,
      observerUpdatedMessage,
      actionStoppedRequestMessage,
      sequenceLoadedMessage,
      sequenceUnloadedMessage,
      resourceBusyMessage,
      modelUpdateMessage,
      defaultMessage).combineAll
}
