// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.{ActionBatch, ActionHandler, ActionResult, Effect, ModelRW, NoAction}
import org.scalajs.dom.window
import seqexec.model.enum.ActionStatus
import seqexec.model.SingleActionOp
import seqexec.model.Observer
import seqexec.model.SequencesQueue
import seqexec.model.SequenceView
import seqexec.model.StepState
import seqexec.model.Step
import seqexec.model.events._
import seqexec.web.client.model.lenses.sequenceStepT
import seqexec.web.client.model.lenses.sequenceViewT
import seqexec.web.client.model.ModelOps._
import seqexec.web.client.model.{AlignAndCalibStep, ResourceRunOperation, SoundSelection}
import seqexec.web.client.actions._
import seqexec.web.client.circuit._
import seqexec.web.client.services.SeqexecWebClient
import seqexec.web.client.services.WebpackResources._
import seqexec.web.client.model.Pages.Root
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.collection.immutable.SortedMap
import web.client.Audio

/**
  * Handles messages received over the WS channel
  */
class ServerMessagesHandler[M](modelRW: ModelRW[M, WebSocketsFocus])
    extends ActionHandler(modelRW)
    with Handlers[M, WebSocketsFocus] {

  // Global references to audio files
  private val SequencePausedAudio = Audio.selectPlayable(
    new Audio(SequencePausedResourceMP3.resource),
    new Audio(SequencePausedResourceWebM.resource))

  private val ExposurePausedAudio = Audio.selectPlayable(
    new Audio(ExposurePausedResourceMP3.resource),
    new Audio(ExposurePausedResourceWebM.resource))

  private val SequenceErrorAudio = Audio.selectPlayable(
    new Audio(SequenceErrorResourceMP3.resource),
    new Audio(SequenceErrorResourceWebM.resource))

  private val SequenceCompleteAudio = Audio.selectPlayable(
    new Audio(SequenceCompleteResourceMP3.resource),
    new Audio(SequenceCompleteResourceWebM.resource))

  private val StepBeepAudio = Audio.selectPlayable(
    new Audio(BeepResourceMP3.resource),
    new Audio(BeepResourceWebM.resource))

  private val ObservationStoppedAudio = Audio.selectPlayable(
    new Audio(ObservationStoppedMP3.resource),
    new Audio(ObservationStoppedWebM.resource))

  private val ObservationAbortedAudio = Audio.selectPlayable(
    new Audio(ObservationAbortedMP3.resource),
    new Audio(ObservationAbortedWebM.resource))

  def loggedIn: Boolean           = value.sound === SoundSelection.SoundOn
  def ifLoggedIn[A]: A => Boolean = (_: A) => loggedIn

  // It is legal to put sequences of the other sites on the queue
  // but we don't know how to display them, so let's filter them out
  private def filterSequences(
    sequences: SequencesQueue[SequenceView]): SequencesQueue[SequenceView] =
    sequences.copy(sessionQueue = sequences.sessionQueue.filter {
      case SequenceView(_, metadata, _, _, _) =>
        value.site
          .exists(_.instruments.toList.contains(metadata.instrument))
    })

  val soundCheck: PartialFunction[Any, ActionResult[M]] = {
    case RequestSoundEcho =>
      val soundEffect = Effect(
        Future(SequenceCompleteAudio.play()).as(NoAction))
      effectOnly(soundEffect)
  }

  val connectionOpenMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(ConnectionOpenEvent(u, c, v)) =>
      // After connected to the Websocket request a refresh
      val refreshRequestE = Effect(SeqexecWebClient.refresh(c).as(NoAction))
      // This is a hack
      val calQueueObserverE = u
        .map(m => Effect(Future(UpdateCalTabObserver(Observer(m.displayName)))))
        .getOrElse(VoidEffect)
      val openEffect =
        if (value.serverVersion.exists(_ =!= v)) {
          Effect(Future(window.location.reload(true)).as(NoAction))
        } else {
          refreshRequestE + calQueueObserverE
        }
      updated(
        value.copy(user = u,
                   defaultObserver = u
                     .map(m => Observer(m.displayName))
                     .getOrElse(value.defaultObserver),
                   clientId      = c.some,
                   serverVersion = v.some),
        openEffect
      )
  }

  val stepCompletedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(e @ StepExecuted(obsId, sv)) =>
      val curStep =
        for {
          obs           <- sequenceViewT.find(_.id === obsId)(e)
          curSIdx       <- obs.runningStep.map(_.last)
          curStep       <- sequenceStepT.find(_.id === curSIdx)(obs)
          observeStatus <- Step.observeStatus.getOption(curStep)
          configStatus  <- Step.configStatus.getOption(curStep)
          d = configStatus // workaround
          if observeStatus === ActionStatus.Pending && curStep.status === StepState.Running
          if configStatus.map(_._2).forall(_ === ActionStatus.Pending)
        } yield curStep

      val doneStep =
        sequenceViewT.find(_.id === obsId)(e).forall(_.runningStep.isEmpty)

      val audioEffect = curStep
        .filter(ifLoggedIn)
        .fold(VoidEffect)(_ =>
          Effect(Future(StepBeepAudio.play()).as(NoAction)))
      val clearAction =
        if (doneStep) Effect(Future(ClearOperations(obsId))) else VoidEffect
      updated(value.copy(sequences = filterSequences(sv)),
              audioEffect + clearAction)
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
    case ServerMessage(SequenceError(id, sv)) =>
      // Play audio when the sequence gets into an error state
      val audioEffect =
        if (loggedIn)
          Effect(Future(SequenceErrorAudio.play()).as(NoAction))
        else VoidEffect
      val clearAction = Effect(Future(ClearRunOnError(id)))
      updated(value.copy(sequences = filterSequences(sv)),
              audioEffect + clearAction)
  }

  val sequencePausedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequencePaused(id, sv)) =>
      // Play audio when the sequence gets paused
      val audioEffect =
        if (loggedIn)
          Effect(Future(SequencePausedAudio.play()).as(NoAction))
        else VoidEffect
      val clearAction = Effect(Future(ClearOperations(id)))
      updated(value.copy(sequences = filterSequences(sv)),
              audioEffect + clearAction)
  }

  val sequencePauseCancelMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequencePauseCanceled(id, sv)) =>
      // Clear operations state
      val clearAction = Effect(Future(ClearOperations(id)))
      updated(value.copy(sequences = filterSequences(sv)), clearAction)
  }

  val sequenceStartMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequenceStart(id, s, sv)) =>
      val updateSelectedStep = Effect(Future(UpdateSelectedStep(id, s)))
      updated(value.copy(sequences = filterSequences(sv)), updateSelectedStep)
  }

  val stopCompletedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequenceStopped(id, sv)) =>
      // A step completed with a stop
      val stopProgress = Effect(Future(RunStopCompleted(id)))
      val stopAudio = Effect(Future(ObservationStoppedAudio.play()).as(NoAction))
      updated(value.copy(sequences = filterSequences(sv)), stopProgress + stopAudio)
  }

  val stepAbortedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(SequenceAborted(id, sv)) =>
      // A step completed with an abort which in practical term is like a stop
      val abortProgress = Effect(Future(RunStopCompleted(id)))
      val abortAudio = Effect(Future(ObservationAbortedAudio.play()).as(NoAction))
      updated(value.copy(sequences = filterSequences(sv)), abortProgress + abortAudio)
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
      val sequences = filterSequences(s.view)
      val resourceRunRequested =
        SequencesQueue.sessionQueueT[SequenceView]
          .getAll(sequences)
          .collect{
            case view if value.resourceRunRequested.keySet.contains(view.id) =>
              view.id ->
                (for {
                  step <- SequenceView.stepT.getAll(view)
                  (resource, status) <- Step.configStatus.getOption(step).orEmpty
                                          if status =!= ActionStatus.Pending
                } yield {
                  ResourceRunOperation
                    .fromActionStatus(step.id)(status)
                    .map((resource, _))
                }).flatten
              }.toMap.mapValues(SortedMap(_:_*))
      updated(value.copy(sequences = sequences, resourceRunRequested = resourceRunRequested))
  }

  val sequenceRefreshedMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(s: SequenceRefreshed) =>
      val clearAction = Effect(Future(ClearAllOperations))
      updated(value.copy(sequences = filterSequences(s.view)), clearAction)
  }

  val MsgRegex  = "Application exception: (.*)".r
  val InstRegex = "Sequence execution failed with error: (.*)".r

  val singleRunCompleteMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(
        SingleActionEvent(SingleActionOp.Completed(sid, stepId, r))) =>
      effectOnly(Effect.action(RunResourceComplete(sid, stepId, r)))

    case ServerMessage(
        SingleActionEvent(SingleActionOp.Started(sid, stepId, r))) =>
      effectOnly(Effect.action(ActionBatch(ClearAllResourceOperationsOnStepChange(sid, stepId), RunResourceRemote(sid, stepId, r))))

    case ServerMessage(
        SingleActionEvent(SingleActionOp.Error(sid, stepId, r, msg))) =>
      // Unbundle the underlying exception message
      val actualMsg = msg match {
        case MsgRegex(m)  => m
        case InstRegex(m) => m
        case m            => m
      }
      effectOnly(Effect(Future(RunResourceFailed(sid, stepId, r, actualMsg))))
  }

  val guideConfigMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(r: GuideConfigUpdate) =>
      updatedL(WebSocketsFocus.guideConfig.set(r.telescope))
  }

  val acMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(AlignAndCalibEvent(i)) =>
      updatedL(WebSocketsFocus.alignAndCalib.set(AlignAndCalibStep.fromInt(i)))
  }

  val defaultMessage: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(_) =>
      // Ignore unknown events
      noChange
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(
      soundCheck,
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
      stopCompletedMessage,
      stepAbortedMessage,
      sequenceStartMessage,
      sequenceRefreshedMessage,
      sequencePauseCancelMessage,
      modelUpdateMessage,
      singleRunCompleteMessage,
      guideConfigMessage,
      acMessage,
      defaultMessage
    ).combineAll
}
