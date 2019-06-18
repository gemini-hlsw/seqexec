// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.model.boopickle

import boopickle.DefaultBasic._
import cats.Eq
import cats.Traverse
import cats.Monoid
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
trait ModelBooPicklers extends GemModelBooPicklers {
  def valuesMap[F[_]: Traverse, A, B](c: F[A], f: A => B): Map[B, A] =
    c.fproduct(f).map(_.swap).toList.toMap

  // scalastyle:off
  def valuesMapPickler[A: Eq, B: Monoid: Pickler](valuesMap: Map[B, A]) =
    transformPickler(
      (t: B) =>
        valuesMap
          .get(t)
          .getOrElse(throw new RuntimeException(s"Failed to decode value")))(
      t => valuesMap.find { case (_, v) => v === t }.map(_._1).orEmpty)
  // scalastyle:on

  val instrumentIdx = valuesMap(Instrument.all, (x: Instrument) => x.ordinal)

  implicit val instrumentPickler = valuesMapPickler(instrumentIdx)

  val resourceIdx =
    valuesMap(Instrument.allResources, (x: Resource) => x.ordinal)

  implicit val resourcePickler = valuesMapPickler(resourceIdx)

  implicit val operatorPickler = generatePickler[Operator]

  val sysNameIdx = valuesMap(SystemName.all, (x: SystemName) => x.system)

  implicit val systemNamePickler = valuesMapPickler(sysNameIdx)

  implicit val observerPickler = generatePickler[Observer]

  implicit val userDetailsPickler = generatePickler[UserDetails]

  implicit val instantPickler =
    transformPickler((t: Long) => Instant.ofEpochMilli(t))(_.toEpochMilli)

  val cloudCoverIdx = valuesMap(CloudCover.all, (x: CloudCover) => x.toInt)

  implicit val cloudCoverPickler = valuesMapPickler(cloudCoverIdx)

  val imageQualityIdx =
    valuesMap(ImageQuality.all, (x: ImageQuality) => x.toInt)

  implicit val imageQualityPickler = valuesMapPickler(imageQualityIdx)

  val skyBackgroundIdx =
    valuesMap(SkyBackground.all, (x: SkyBackground) => x.toInt)

  implicit val skyBackgroundPickler = valuesMapPickler(skyBackgroundIdx)

  val waterVaporIdx = valuesMap(WaterVapor.all, (x: WaterVapor) => x.toInt)

  implicit val waterVaporPickler = valuesMapPickler(waterVaporIdx)

  implicit val sequenceStateCompletedPickler =
    generatePickler[SequenceState.Completed.type]
  implicit val sequenceStateIdlePickler =
    generatePickler[SequenceState.Idle.type]
  implicit val sequenceStateRunningPickler =
    generatePickler[SequenceState.Running]
  implicit val sequenceStateFailedPickler =
    generatePickler[SequenceState.Failed]

  implicit val sequenceStatePickler = compositePickler[SequenceState]
    .addConcreteType[SequenceState.Completed.type]
    .addConcreteType[SequenceState.Running]
    .addConcreteType[SequenceState.Failed]
    .addConcreteType[SequenceState.Idle.type]

  private val actionStatusIdx = Map((0 -> ActionStatus.Pending),
                                    (1 -> ActionStatus.Completed),
                                    (2 -> ActionStatus.Running),
                                    (3 -> ActionStatus.Paused),
                                    (4 -> ActionStatus.Failed))

  implicit val actionStatusPickler = valuesMapPickler(actionStatusIdx)

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

  implicit val singleActionOpStartedPickler   = generatePickler[SingleActionOp.Started]
  implicit val singleActionOpCompletedPickler = generatePickler[SingleActionOp.Completed]
  implicit val singleActionOpErrorPickler     = generatePickler[SingleActionOp.Error]
  implicit val singleActionOpPickler = compositePickler[SingleActionOp]
    .addConcreteType[SingleActionOp.Started]
    .addConcreteType[SingleActionOp.Completed]
    .addConcreteType[SingleActionOp.Error]

  private val serverLogIdx = Map((0 -> ServerLogLevel.INFO),
                                 (1 -> ServerLogLevel.WARN),
                                 (2 -> ServerLogLevel.ERROR))

  implicit val serverLogLevelPickler = valuesMapPickler(serverLogIdx)

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

  implicit val batchExecStatePickler = valuesMapPickler(batchExecStateIdx)

  implicit val executionQueuePickler = generatePickler[ExecutionQueueView]

  implicit val conditionsPickler = generatePickler[Conditions]

  implicit val sequenceQueueIdPickler =
    generatePickler[SequencesQueue[Observation.Id]]

  implicit val sequenceQueueViewPickler =
    generatePickler[SequencesQueue[SequenceView]]

  private val comaOptionIdx = Map((0 -> ComaOption.ComaOn),
                                  (1 -> ComaOption.ComaOff))

  implicit val comaPickler = valuesMapPickler(comaOptionIdx)

  private val tipTiltSourceIdx = Map((0 -> TipTiltSource.PWFS1),
                                     (1 -> TipTiltSource.PWFS2),
                                     (2 -> TipTiltSource.OIWFS),
                                     (3 -> TipTiltSource.GAOS))

  implicit val tipTiltSourcePickler = valuesMapPickler(tipTiltSourceIdx)

  private val m1SourceIdx = Map((0 -> M1Source.PWFS1),
                                (1 -> M1Source.PWFS2),
                                (2 -> M1Source.OIWFS),
                                (3 -> M1Source.GAOS),
                                (4 -> M1Source.HRWFS))

  implicit val m1SourcePickler = valuesMapPickler(m1SourceIdx)

  private val mountGuideIdx = Map((0 -> MountGuideOption.MountGuideOff),
                                  (1 -> MountGuideOption.MountGuideOn))

  implicit val mountGuidePickler = valuesMapPickler(mountGuideIdx)
  implicit val m1GuideOnPickler  = generatePickler[M1GuideConfig.M1GuideOn]
  implicit val m1GuideOffPickler =
    generatePickler[M1GuideConfig.M1GuideOff.type]
  implicit val m1GuideConfigPickler = compositePickler[M1GuideConfig]
    .addConcreteType[M1GuideConfig.M1GuideOn]
    .addConcreteType[M1GuideConfig.M1GuideOff.type]

  implicit val m2GuideOnPickler = generatePickler[M2GuideConfig.M2GuideOn]
  implicit val m2GuideOffPickler =
    generatePickler[M2GuideConfig.M2GuideOff.type]
  implicit val m2GuideConfigPickler = compositePickler[M2GuideConfig]
    .addConcreteType[M2GuideConfig.M2GuideOn]
    .addConcreteType[M2GuideConfig.M2GuideOff.type]

  implicit val telescopeGuideconfigPickler =
    generatePickler[TelescopeGuideConfig]

  implicit val resourceConflictPickler = generatePickler[ResourceConflict]
  implicit val instrumentInUsePickler  = generatePickler[InstrumentInUse]
  implicit val requestFailedPickler    = generatePickler[RequestFailed]
  implicit val subsystemlBusyPickler   = generatePickler[SubsystemBusy]
  implicit val notificatonPickler: Pickler[Notification] =
    compositePickler[Notification]
      .addConcreteType[ResourceConflict]
      .addConcreteType[InstrumentInUse]
      .addConcreteType[RequestFailed]
      .addConcreteType[SubsystemBusy]

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
  implicit val sequenceStoppedPickler     = generatePickler[SequenceStopped]
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
  implicit val singleActionEventPickler   = generatePickler[SingleActionEvent]
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
    .addConcreteType[SequenceStopped]
    .addConcreteType[SequenceUpdated]
    .addConcreteType[SequenceError]
    .addConcreteType[SequencePaused]
    .addConcreteType[ExposurePaused]
    .addConcreteType[ServerLogMessage]
    .addConcreteType[UserNotification]
    .addConcreteType[QueueUpdated]
    .addConcreteType[ObservationProgressEvent]
    .addConcreteType[SingleActionEvent]
    .addConcreteType[NullEvent.type]

  implicit val userLoginPickler = generatePickler[UserLoginRequest]

}
