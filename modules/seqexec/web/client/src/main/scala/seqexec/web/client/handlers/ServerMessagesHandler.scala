// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import java.util.logging.Logger

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.ModelRW
import diode.NoAction
import seqexec.model.enum.{ActionStatus, SingleActionOp}
import seqexec.model.Observer
import seqexec.model.SequencesQueue
import seqexec.model.SequenceView
import seqexec.model.StepState
import seqexec.model.events._
import seqexec.web.client.model.lenses.sequenceStepT
import seqexec.web.client.model.lenses.sequenceViewT
import seqexec.web.client.model.ModelOps._
import seqexec.web.client.model.SoundSelection
import seqexec.web.client.actions._
import seqexec.web.client.circuit._
import seqexec.web.client.services.Audio
import seqexec.web.client.services.SeqexecWebClient
import seqexec.web.client.services.WebpackResources._
import seqexec.web.client.model.Pages.Root

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
  * Handles messages received over the WS channel
  */
class ServerMessagesHandler[M](modelRW: ModelRW[M, WebSocketsFocus])
    extends ActionHandler(modelRW)
    with Handlers[M, WebSocketsFocus] {
  private val logger = Logger.getLogger(this.getClass.getName)
  // Global references to audio files
  private val SequencePausedAudio = new Audio(SequencePausedResource.resource)
  private val ExposurePausedAudio = new Audio(ExposurePausedResource.resource)
  private val SequenceErrorAudio  = new Audio(SequenceErrorResource.resource)
  private val SequenceCompleteAudio = new Audio(
    SequenceCompleteResource.resource)
  private val StepBeepAudio = new Audio(BeepResource.resource)

  def loggedIn: Boolean           = value.sound === SoundSelection.SoundOn
  def ifLoggedIn[A]: A => Boolean = (_: A) => loggedIn

  // It is legal do put sequences of the other sites on the queue
  // but we don't know how to display them, so let's filter them out
  private def filterSequences(
    sequences: SequencesQueue[SequenceView]): SequencesQueue[SequenceView] =
    sequences.copy(sessionQueue = sequences.sessionQueue.filter {
      case SequenceView(_, metadata, _, _, _) =>
        value.site
          .map(_.instruments.toList.contains(metadata.instrument))
          .getOrElse(false)
    })

  val soundCheck: PartialFunction[Any, ActionResult[M]] = {
    case RequestSoundEcho =>
      val soundEffect = Effect(
        Future(SequenceCompleteAudio.play()).as(NoAction))
      effectOnly(soundEffect)
  }

  val logMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(l: ServerLogMessage) =>
      effectOnly(Effect(Future(AppendToLog(l))))
  }

  val connectionOpenMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(ConnectionOpenEvent(u, c)) =>
      // After connected to the Websocket request a refresh
      val refreshRequestE = Effect(SeqexecWebClient.refresh(c).as(NoAction))
      // This is a hack
      val calQueueObserverE = u
        .map(m => Effect(Future(UpdateCalTabObserver(Observer(m.displayName)))))
        .getOrElse(VoidEffect)
      updated(
        value.copy(user = u,
                   defaultObserver = u
                     .map(m => Observer(m.displayName))
                     .getOrElse(value.defaultObserver),
                   clientId = Option(c)),
        refreshRequestE + calQueueObserverE
      )
  }

  val stepCompletedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(e @ StepExecuted(obsId, sv)) =>
      val curStep =
        for {
          obs     <- sequenceViewT.find(_.id === obsId)(e)
          curSIdx <- obs.runningStep.map(_.last)
          curStep <- sequenceStepT.find(_.id === curSIdx)(obs)
          if curStep.observeStatus === ActionStatus.Pending && curStep.status === StepState.Running
          if curStep.configStatus.map(_._2).forall(_ === ActionStatus.Pending)
        } yield curStep

      val audioEffect = curStep
        .filter(ifLoggedIn)
        .fold(VoidEffect)(_ =>
          Effect(Future(StepBeepAudio.play()).as(NoAction)))
      updated(value.copy(sequences = filterSequences(sv)), audioEffect)
  }

  val sequenceCompletedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequenceCompleted(sv)) =>
      // Play audio when the sequence completes
      val audioEffect =
        if (loggedIn)
          Effect(Future(SequenceCompleteAudio.play()).as(NoAction))
        else VoidEffect
      updated(value.copy(sequences = filterSequences(sv)), audioEffect)
  }

  val sequenceUnloadedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequenceUnloaded(id, sv))
        if value.sequences.sessionQueue.map(_.id).contains(id) =>
      updated(value.copy(sequences = filterSequences(sv)),
              Effect(Future(NavigateTo(Root))))
  }

  val sequenceOnErrorMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequenceError(_, sv)) =>
      // Play audio when the sequence gets into an error state
      val audioEffect =
        if (loggedIn)
          Effect(Future(SequenceErrorAudio.play()).as(NoAction))
        else VoidEffect
      updated(value.copy(sequences = filterSequences(sv)), audioEffect)
  }

  val sequencePausedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequencePaused(_, sv)) =>
      // Play audio when the sequence gets paused
      val audioEffect =
        if (loggedIn)
          Effect(Future(SequencePausedAudio.play()).as(NoAction))
        else VoidEffect
      updated(value.copy(sequences = filterSequences(sv)), audioEffect)
  }

  val exposurePausedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(ExposurePaused(_, sv)) =>
      // Play audio when the sequence gets paused
      val audioEffect =
        if (loggedIn)
          Effect(Future(ExposurePausedAudio.play()).as(NoAction))
        else VoidEffect
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

  val sequenceLoadedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequenceLoaded(_, view)) =>
      updated(value.copy(sequences = filterSequences(view)))
  }

  val modelUpdateMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(s: SeqexecModelUpdate) =>
      updated(value.copy(sequences = filterSequences(s.view)))
  }

  val singleRunCompleteMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SingleActionEvent(SingleActionOp.Completed(sid, stepId, r))) =>
      logger.info(s"*** ServerMessagesHandler singleRunCompletedMessage: r=$r")
      effectOnly(Effect(Future(RunResource(sid, stepId, r))))
    //      val v = value.markOperations(id, TabOperations.resourceRun(r).set(none))
    //      effectOnly(Effect(Future(v))(?: ActionType[SequencesOnDisplay], queue))
  }

  val defaultMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(_) =>
      // Ignore unknown events
      noChange
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(
      soundCheck,
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
      modelUpdateMessage,
      singleRunCompleteMessage,
      defaultMessage
    ).combineAll
}
