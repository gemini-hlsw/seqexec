// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.model.boopickle

import boopickle.DefaultBasic._
import cats.Traverse
import cats.implicits._
import gem.Observation
import java.time.Instant
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.events._
import squants.time.TimeConversions._

/**
  * Contains boopickle implicit picklers of model objects
  * Boopickle can auto derived encoders but it is preferred to make
  * them explicitly
  */
@SuppressWarnings(
  Array(
    "org.wartremover.warts.Equals",
    "org.wartremover.warts.PublicInference",
    "org.wartremover.warts.ImplicitParameter",
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.Throw",
    "org.wartremover.warts.OptionPartial"
  ))
trait ModelBooPicklers extends GemModelBooPicklers {
  def valuesMap[F[_]: Traverse, A, B](c: F[A], f: A => B): Map[B, A] =
    c.fproduct(f).map(_.swap).toList.toMap

  val instrumentIdx = valuesMap(Instrument.all, (x: Instrument) => x.ordinal)

  implicit val instrumentPickler = transformPickler(
    (t: Int) =>
      instrumentIdx
        .get(t)
        .getOrElse(throw new RuntimeException("Failed to decode instrument")))(
    _.ordinal)

  val resourceIdx =
    valuesMap(Instrument.allResources, (x: Resource) => x.ordinal)

  implicit val resourcePickler = transformPickler(
    (t: Int) =>
      resourceIdx
        .get(t)
        .getOrElse(throw new RuntimeException("Failed to decode resource")))(
    _.ordinal)

  implicit val operatorPickler = generatePickler[Operator]

  val sysNameIdx = valuesMap(SystemName.all, (x: SystemName) => x.system)

  implicit val systemNamePickler = transformPickler(
    (t: String) =>
      sysNameIdx
        .get(t)
        .getOrElse(throw new RuntimeException("Failed to decode system name")))(
    _.system)

  implicit val observerPickler = generatePickler[Observer]

  implicit val userDetailsPickler = generatePickler[UserDetails]

  implicit val instantPickler =
    transformPickler((t: Long) => Instant.ofEpochMilli(t))(_.toEpochMilli)

  val cloudCoverIdx = valuesMap(CloudCover.all, (x: CloudCover) => x.toInt)

  implicit val cloudCoverPickler = transformPickler(
    (t: Int) =>
      cloudCoverIdx
        .get(t)
        .getOrElse(throw new RuntimeException("Failed to decode cloud cover")))(
    _.toInt)

  val imageQualityIdx =
    valuesMap(ImageQuality.all, (x: ImageQuality) => x.toInt)

  implicit val imageQualityPickler = transformPickler((t: Int) =>
    imageQualityIdx
      .get(t)
      .getOrElse(throw new RuntimeException("Failed to decode image quality")))(
    _.toInt)

  val skyBackgroundIdx =
    valuesMap(SkyBackground.all, (x: SkyBackground) => x.toInt)

  implicit val skyBackgroundPickler = transformPickler(
    (t: Int) =>
      skyBackgroundIdx
        .get(t)
        .getOrElse(
          throw new RuntimeException("Failed to decode sky background")))(
    _.toInt)

  val waterVaporIdx = valuesMap(WaterVapor.all, (x: WaterVapor) => x.toInt)

  implicit val waterVaporPickler = transformPickler(
    (t: Int) =>
      waterVaporIdx
        .get(t)
        .getOrElse(throw new RuntimeException("Failed to decode water vapor")))(
    _.toInt)

  implicit val sequenceStateCompletedPickler =
    generatePickler[SequenceState.Completed.type]
  implicit val sequenceStateIdlePickler =
    generatePickler[SequenceState.Idle.type]
  implicit val sequenceStateStoppedPickler =
    generatePickler[SequenceState.Stopped.type]
  implicit val sequenceStateRunningPickler =
    generatePickler[SequenceState.Running]
  implicit val sequenceStateFailedPickler =
    generatePickler[SequenceState.Failed]

  implicit val sequenceStatePickler = compositePickler[SequenceState]
    .addConcreteType[SequenceState.Completed.type]
    .addConcreteType[SequenceState.Running]
    .addConcreteType[SequenceState.Failed]
    .addConcreteType[SequenceState.Stopped.type]
    .addConcreteType[SequenceState.Idle.type]

  private val actionStatusIdx = Map((0 -> ActionStatus.Pending),
                                    (1 -> ActionStatus.Completed),
                                    (2 -> ActionStatus.Running),
                                    (3 -> ActionStatus.Paused),
                                    (4 -> ActionStatus.Failed))

  implicit val actionStatusPickler = transformPickler(
    (t: Int) =>
      actionStatusIdx
        .get(t)
        .getOrElse(
          throw new RuntimeException("Falied to decode action status")))(t =>
    actionStatusIdx.find { case (_, v) => v === t }.map(_._1).getOrElse(-1))

  implicit val stepStatePendingPickler = generatePickler[StepState.Pending.type]
  implicit val stepStateCompletedPickler =
    generatePickler[StepState.Completed.type]
  implicit val stepStateSkippedPickler = generatePickler[StepState.Skipped.type]
  implicit val stepStateFailedPickler  = generatePickler[StepState.Failed]
  implicit val stepStateRunningPickler = generatePickler[StepState.Running.type]
  implicit val stepStatePausedPickler  = generatePickler[StepState.Paused.type]

  implicit val stepStatePickler = compositePickler[StepState]
    .addConcreteType[StepState.Pending.type]
    .addConcreteType[StepState.Completed.type]
    .addConcreteType[StepState.Skipped.type]
    .addConcreteType[StepState.Failed]
    .addConcreteType[StepState.Running.type]
    .addConcreteType[StepState.Paused.type]

  implicit val standardStepPickler = generatePickler[StandardStep]

  implicit val stepPickler = compositePickler[Step]
    .addConcreteType[StandardStep]

  implicit val sequenceMetadataPickler = generatePickler[SequenceMetadata]

  implicit val stepConfigPickler = generatePickler[SequenceView]
  implicit val clientIdPickler   = generatePickler[ClientId]

  implicit val queueIdPickler      = generatePickler[QueueId]
  implicit val queueOpMovedPickler = generatePickler[QueueManipulationOp.Moved]
  implicit val queueOpStartedPickler =
    generatePickler[QueueManipulationOp.Started]
  implicit val queueOpStoppedPickler =
    generatePickler[QueueManipulationOp.Stopped]
  implicit val queueOpClearPickler = generatePickler[QueueManipulationOp.Clear]
  implicit val queueOpAddedSeqsPickler =
    generatePickler[QueueManipulationOp.AddedSeqs]
  implicit val queueOpRemovedSeqsPickler =
    generatePickler[QueueManipulationOp.RemovedSeqs]

  implicit val queueOpPickler = compositePickler[QueueManipulationOp]
    .addConcreteType[QueueManipulationOp.Clear]
    .addConcreteType[QueueManipulationOp.Started]
    .addConcreteType[QueueManipulationOp.Stopped]
    .addConcreteType[QueueManipulationOp.Moved]
    .addConcreteType[QueueManipulationOp.AddedSeqs]
    .addConcreteType[QueueManipulationOp.RemovedSeqs]

  private val serverLogIdx = Map((0 -> ServerLogLevel.INFO),
                                 (1 -> ServerLogLevel.WARN),
                                 (2 -> ServerLogLevel.ERROR))

  implicit val serverLogLevelPickler = transformPickler(
    (t: Int) =>
      serverLogIdx
        .get(t)
        .getOrElse(
          throw new RuntimeException("Falied to decode server log level")))(t =>
    serverLogIdx.find { case (_, v) => v === t }.map(_._1).getOrElse(-1))

  implicit val batchCommandStateIdlePickler =
    generatePickler[BatchCommandState.Idle.type]
  implicit val batchCommandStateRun = generatePickler[BatchCommandState.Run]
  implicit val batchCommandStateStopPickler =
    generatePickler[BatchCommandState.Stop.type]

  implicit val batchCommandPickler = compositePickler[BatchCommandState]
    .addConcreteType[BatchCommandState.Idle.type]
    .addConcreteType[BatchCommandState.Run]
    .addConcreteType[BatchCommandState.Stop.type]

  private val batchExecStateIdx = Map((0 -> BatchExecState.Idle),
                                      (1 -> BatchExecState.Running),
                                      (2 -> BatchExecState.Waiting),
                                      (3 -> BatchExecState.Stopping),
                                      (4 -> BatchExecState.Completed))

  implicit val batchExecStatePickler = transformPickler(
    (t: Int) =>
      batchExecStateIdx
        .get(t)
        .getOrElse(
          throw new RuntimeException("Falied to decode batch exec state")))(t =>
    batchExecStateIdx.find { case (_, v) => v === t }.map(_._1).getOrElse(-1))

  implicit val executionQueuePickler = generatePickler[ExecutionQueueView]

  implicit val conditionsPickler = generatePickler[Conditions]

  implicit val sequenceQueueIdPickler =
    generatePickler[SequencesQueue[Observation.Id]]

  implicit val sequenceQueueViewPickler =
    generatePickler[SequencesQueue[SequenceView]]

  implicit val resourceConflictPickler = generatePickler[ResourceConflict]
  implicit val instrumentInUsePickler  = generatePickler[InstrumentInUse]
  implicit val requestFailedPickler    = generatePickler[RequestFailed]
  implicit val notificatonPickler: Pickler[Notification] =
    compositePickler[Notification]
      .addConcreteType[ResourceConflict]
      .addConcreteType[InstrumentInUse]
      .addConcreteType[RequestFailed]

  implicit val connectionOpenEventPickler = generatePickler[ConnectionOpenEvent]
  implicit val sequenceStartPickler       = generatePickler[SequenceStart]
  implicit val stepExecutedPickler        = generatePickler[StepExecuted]
  implicit val fileIdStepExecutedPickler  = generatePickler[FileIdStepExecuted]
  implicit val sequenceCompletedPickler   = generatePickler[SequenceCompleted]
  implicit val sequenceLoadedPickler      = generatePickler[SequenceLoaded]
  implicit val sequenceUnloadedPickler    = generatePickler[SequenceUnloaded]
  implicit val stepBreakpointChangedPickler =
    generatePickler[StepBreakpointChanged]
  implicit val operatorUpdatedPickler     = generatePickler[OperatorUpdated]
  implicit val observerUpdatedPickler     = generatePickler[ObserverUpdated]
  implicit val conditionsUpdatedPickler   = generatePickler[ConditionsUpdated]
  implicit val loadSequenceUpdatedPickler = generatePickler[LoadSequenceUpdated]
  implicit val clearLoadedSequencesUpdatedPickler =
    generatePickler[ClearLoadedSequencesUpdated]
  implicit val stepSkipMarkChangedPickler = generatePickler[StepSkipMarkChanged]
  implicit val sequencePauseRequestedPickler =
    generatePickler[SequencePauseRequested]
  implicit val sequencePauseCanceledPickler =
    generatePickler[SequencePauseCanceled]
  implicit val sequenceRefreshedPickler   = generatePickler[SequenceRefreshed]
  implicit val actionStopRequestedPickler = generatePickler[ActionStopRequested]
  implicit val sequenceUpdatedPickler     = generatePickler[SequenceUpdated]
  implicit val sequenceErrorPickler       = generatePickler[SequenceError]
  implicit val sequencePausedPickler      = generatePickler[SequencePaused]
  implicit val exposurePausedPickler      = generatePickler[ExposurePaused]
  implicit val serverLogMessagePickler    = generatePickler[ServerLogMessage]
  implicit val userNotificationPickler    = generatePickler[UserNotification]
  implicit val queueUpdatedPickler        = generatePickler[QueueUpdated]
  implicit val timeProgressPickler =
    transformPickler((t: Double) => t.milliseconds)(_.toMilliseconds)
  implicit val observationProgressPickler = generatePickler[ObservationProgress]
  implicit val obsProgressPickler         = generatePickler[ObservationProgressEvent]
  implicit val nullEventPickler           = generatePickler[NullEvent.type]

  // Composite pickler for the seqexec event hierarchy
  implicit val eventsPickler = compositePickler[SeqexecEvent]
    .addConcreteType[ConnectionOpenEvent]
    .addConcreteType[SequenceStart]
    .addConcreteType[StepExecuted]
    .addConcreteType[FileIdStepExecuted]
    .addConcreteType[SequenceCompleted]
    .addConcreteType[SequenceLoaded]
    .addConcreteType[SequenceUnloaded]
    .addConcreteType[StepBreakpointChanged]
    .addConcreteType[OperatorUpdated]
    .addConcreteType[ObserverUpdated]
    .addConcreteType[ConditionsUpdated]
    .addConcreteType[LoadSequenceUpdated]
    .addConcreteType[ClearLoadedSequencesUpdated]
    .addConcreteType[StepSkipMarkChanged]
    .addConcreteType[SequencePauseRequested]
    .addConcreteType[SequencePauseCanceled]
    .addConcreteType[SequenceRefreshed]
    .addConcreteType[ActionStopRequested]
    .addConcreteType[SequenceUpdated]
    .addConcreteType[SequenceError]
    .addConcreteType[SequencePaused]
    .addConcreteType[ExposurePaused]
    .addConcreteType[ServerLogMessage]
    .addConcreteType[UserNotification]
    .addConcreteType[QueueUpdated]
    .addConcreteType[ObservationProgressEvent]
    .addConcreteType[NullEvent.type]

  implicit val userLoginPickler = generatePickler[UserLoginRequest]

}
