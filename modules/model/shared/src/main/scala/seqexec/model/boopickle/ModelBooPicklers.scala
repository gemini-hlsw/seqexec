// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.boopickle

import java.time._
import boopickle.Pickler
import boopickle.CompositePickler
import boopickle.Default.UUIDPickler
import boopickle.Default.booleanPickler
import boopickle.Default.compositePickler
import boopickle.Default.doublePickler
import boopickle.Default.generatePickler
import boopickle.Default.intPickler
import boopickle.Default.longPickler
import boopickle.Default.optionPickler
import boopickle.Default.shortPickler
import boopickle.Default.stringPickler
import boopickle.Default.transformPickler
import boopickle.DefaultBasic.iterablePickler
import boopickle.DefaultBasic.mapPickler
import cats._
import cats.syntax.all._
import lucuma.core.math.Index
import lucuma.core.util.Enumerated
import seqexec.model.GmosParameters._
import seqexec.model.NodAndShuffleStep.PendingObserveCmd
import seqexec.model.Observation
import seqexec.model.UserPrompt.ChecksOverride
import seqexec.model.UserPrompt.SeqCheck
import seqexec.model._
import seqexec.model.dhs._
import seqexec.model.enum._
import seqexec.model.events._
import shapeless.tag
import shapeless.tag.@@
import squants.time.Time
import squants.time.TimeConversions._

/**
 * Contains boopickle implicit picklers of model objects Boopickle can auto derive encoders but it
 * is preferred to make them explicitly
 */
trait ModelBooPicklers extends BooPicklerSyntax {
  implicit val yearPickler: Pickler[Year]                           = transformPickler(Year.of)(_.getValue)
  implicit val localDatePickler: Pickler[LocalDate]                 =
    transformPickler(LocalDate.ofEpochDay)(_.toEpochDay)
  implicit val programIdPickler: Pickler[ProgramId]                 = ProgramId.fromString.toPickler
  implicit val indexPickler: Pickler[Index]                         = Index.fromShort.toPickler
  implicit val observationIdPickler: Pickler[Observation.Id]        = generatePickler[Observation.Id]
  implicit val lObservationIdPickler: Pickler[List[Observation.Id]] =
    iterablePickler[Observation.Id, List]

  def valuesMap[F[_]: Traverse, A, B](c: F[A], f: A => B): Map[B, A] =
    c.fproduct(f).map(_.swap).toList.toMap

  def sourceIndex[A: Enumerated]: Map[Int, A] =
    Enumerated[A].all.zipWithIndex.map(_.swap).toMap

  def valuesMapPickler[A: Enumerated, B: Monoid: Pickler](
    valuesMap: Map[B, A]
  ): Pickler[A] =
    transformPickler((t: B) =>
      valuesMap
        .get(t)
        .getOrElse(throw new RuntimeException(s"Failed to decode value"))
    )(t => valuesMap.find { case (_, v) => v === t }.foldMap(_._1))

  def enumeratedPickler[A: Enumerated]: Pickler[A] =
    valuesMapPickler[A, Int](sourceIndex[A])

  implicit val timeProgressPickler: Pickler[Time] =
    transformPickler((t: Double) => t.milliseconds)(_.toMilliseconds)

  implicit val instrumentPickler: Pickler[Instrument] = enumeratedPickler[Instrument]
  implicit val resourcePickler: Pickler[Resource]     = enumeratedPickler[Resource]

  implicit val operatorPickler: Pickler[Operator]         = generatePickler[Operator]
  implicit val overridesPickler: Pickler[SystemOverrides] = generatePickler[SystemOverrides]

  implicit val systemNamePickler: Pickler[SystemName] = enumeratedPickler[SystemName]

  implicit val observerPickler: Pickler[Observer] = generatePickler[Observer]

  implicit val userDetailsPickler: Pickler[UserDetails] = generatePickler[UserDetails]

  implicit val instantPickler: Pickler[Instant] =
    transformPickler((t: Long) => Instant.ofEpochMilli(t))(_.toEpochMilli)

  implicit val cloudCoverPickler: Pickler[CloudCover]       = enumeratedPickler[CloudCover]
  implicit val imageQualityPickler: Pickler[ImageQuality]   = enumeratedPickler[ImageQuality]
  implicit val skyBackgroundPickler: Pickler[SkyBackground] = enumeratedPickler[SkyBackground]
  implicit val waterVaporPickler: Pickler[WaterVapor]       = enumeratedPickler[WaterVapor]
  implicit val conditionsPickler: Pickler[Conditions]       = generatePickler[Conditions]

  implicit val sequenceStateCompletedPickler: Pickler[SequenceState.Completed.type] =
    generatePickler[SequenceState.Completed.type]
  implicit val sequenceStateIdlePickler: Pickler[SequenceState.Idle.type]           =
    generatePickler[SequenceState.Idle.type]
  implicit val sequenceStateRunningPickler: Pickler[SequenceState.Running]          =
    generatePickler[SequenceState.Running]
  implicit val sequenceStateFailedPickler: Pickler[SequenceState.Failed]            =
    generatePickler[SequenceState.Failed]
  implicit val sequenceStateAbortedPickler: Pickler[SequenceState.Aborted.type]     =
    generatePickler[SequenceState.Aborted.type]

  implicit val sequenceStatePickler: CompositePickler[SequenceState] =
    compositePickler[SequenceState]
      .addConcreteType[SequenceState.Completed.type]
      .addConcreteType[SequenceState.Running]
      .addConcreteType[SequenceState.Failed]
      .addConcreteType[SequenceState.Aborted.type]
      .addConcreteType[SequenceState.Idle.type]

  implicit val actionStatusPickler: Pickler[ActionStatus] = enumeratedPickler[ActionStatus]

  implicit val stepStatePendingPickler: Pickler[StepState.Pending.type]     =
    generatePickler[StepState.Pending.type]
  implicit val stepStateCompletedPickler: Pickler[StepState.Completed.type] =
    generatePickler[StepState.Completed.type]
  implicit val stepStateSkippedPickler: Pickler[StepState.Skipped.type]     =
    generatePickler[StepState.Skipped.type]
  implicit val stepStateFailedPickler: Pickler[StepState.Failed]            = generatePickler[StepState.Failed]
  implicit val stepStateRunningPickler: Pickler[StepState.Running.type]     =
    generatePickler[StepState.Running.type]
  implicit val stepStatePausedPickler: Pickler[StepState.Paused.type]       =
    generatePickler[StepState.Paused.type]
  implicit val stepStateAbortedPickler: Pickler[StepState.Aborted.type]     =
    generatePickler[StepState.Aborted.type]

  implicit val stepStatePickler: CompositePickler[StepState] = compositePickler[StepState]
    .addConcreteType[StepState.Pending.type]
    .addConcreteType[StepState.Completed.type]
    .addConcreteType[StepState.Skipped.type]
    .addConcreteType[StepState.Failed]
    .addConcreteType[StepState.Aborted.type]
    .addConcreteType[StepState.Running.type]
    .addConcreteType[StepState.Paused.type]

  implicit val imageIdPickler: Pickler[String @@ ImageFileIdT] =
    transformPickler((s: String) => tag[ImageFileIdT](s))(identity)
  implicit val standardStepPickler: Pickler[StandardStep]      = generatePickler[StandardStep]
  implicit def taggedIntPickler[A]: Pickler[Int @@ A]          =
    transformPickler((s: Int) => tag[A](s))(identity)
  implicit val nsStagePickler: Pickler[NodAndShuffleStage]     = enumeratedPickler[NodAndShuffleStage]
  implicit val nsActionPickler: Pickler[NSAction]              = enumeratedPickler[NSAction]
  implicit val nsSubexposurePickler: Pickler[NSSubexposure]    =
    transformPickler[NSSubexposure, (NsCycles, NsCycles, Int)] {
      case (t: NsCycles, c: NsCycles, i: Int) =>
        NSSubexposure
          .apply(t, c, i)
          .getOrElse(
            throw new RuntimeException("Failed to decode ns subexposure")
          )
      case _                                  => throw new RuntimeException("Failed to decode ns subexposure")
    }((ns: NSSubexposure) => (ns.totalCycles, ns.cycle, ns.stageIndex))
  implicit val nsRunningStatePickler: Pickler[NSRunningState]  = generatePickler[NSRunningState]
  implicit val nsStatusPickler: Pickler[NodAndShuffleStatus]   = generatePickler[NodAndShuffleStatus]
  implicit val nsPendObsCmdPickler: Pickler[PendingObserveCmd] =
    enumeratedPickler[PendingObserveCmd]
  implicit val nsStepPickler: Pickler[NodAndShuffleStep]       = generatePickler[NodAndShuffleStep]

  implicit val stepPickler: CompositePickler[Step] = compositePickler[Step]
    .addConcreteType[StandardStep]
    .addConcreteType[NodAndShuffleStep]

  implicit val sequenceMetadataPickler: Pickler[SequenceMetadata] =
    generatePickler[SequenceMetadata]

  implicit val stepConfigPickler: Pickler[SequenceView] = generatePickler[SequenceView]
  implicit val clientIdPickler: Pickler[ClientId]       = generatePickler[ClientId]

  implicit val queueIdPickler: Pickler[QueueId]                                    = generatePickler[QueueId]
  implicit val queueOpMovedPickler: Pickler[QueueManipulationOp.Moved]             =
    generatePickler[QueueManipulationOp.Moved]
  implicit val queueOpStartedPickler: Pickler[QueueManipulationOp.Started]         =
    generatePickler[QueueManipulationOp.Started]
  implicit val queueOpStoppedPickler: Pickler[QueueManipulationOp.Stopped]         =
    generatePickler[QueueManipulationOp.Stopped]
  implicit val queueOpClearPickler: Pickler[QueueManipulationOp.Clear]             =
    generatePickler[QueueManipulationOp.Clear]
  implicit val queueOpAddedSeqsPickler: Pickler[QueueManipulationOp.AddedSeqs]     =
    generatePickler[QueueManipulationOp.AddedSeqs]
  implicit val queueOpRemovedSeqsPickler: Pickler[QueueManipulationOp.RemovedSeqs] =
    generatePickler[QueueManipulationOp.RemovedSeqs]

  implicit val queueOpPickler: CompositePickler[QueueManipulationOp] =
    compositePickler[QueueManipulationOp]
      .addConcreteType[QueueManipulationOp.Clear]
      .addConcreteType[QueueManipulationOp.Started]
      .addConcreteType[QueueManipulationOp.Stopped]
      .addConcreteType[QueueManipulationOp.Moved]
      .addConcreteType[QueueManipulationOp.AddedSeqs]
      .addConcreteType[QueueManipulationOp.RemovedSeqs]

  implicit val singleActionOpStartedPickler: Pickler[SingleActionOp.Started]     =
    generatePickler[SingleActionOp.Started]
  implicit val singleActionOpCompletedPickler: Pickler[SingleActionOp.Completed] =
    generatePickler[SingleActionOp.Completed]
  implicit val singleActionOpErrorPickler: Pickler[SingleActionOp.Error]         =
    generatePickler[SingleActionOp.Error]
  implicit val singleActionOpPickler: CompositePickler[SingleActionOp]           =
    compositePickler[SingleActionOp]
      .addConcreteType[SingleActionOp.Started]
      .addConcreteType[SingleActionOp.Completed]
      .addConcreteType[SingleActionOp.Error]

  implicit val batchCommandStateIdlePickler: Pickler[BatchCommandState.Idle.type] =
    generatePickler[BatchCommandState.Idle.type]
  implicit val batchCommandStateRun: Pickler[BatchCommandState.Run]               =
    generatePickler[BatchCommandState.Run]
  implicit val batchCommandStateStopPickler: Pickler[BatchCommandState.Stop.type] =
    generatePickler[BatchCommandState.Stop.type]

  implicit val batchCommandPickler: CompositePickler[BatchCommandState] =
    compositePickler[BatchCommandState]
      .addConcreteType[BatchCommandState.Idle.type]
      .addConcreteType[BatchCommandState.Run]
      .addConcreteType[BatchCommandState.Stop.type]

  implicit val batchExecStatePickler: Pickler[BatchExecState] = enumeratedPickler[BatchExecState]

  implicit val executionQueuePickler: Pickler[ExecutionQueueView] =
    generatePickler[ExecutionQueueView]

  implicit val sequenceQueueIdPickler: Pickler[SequencesQueue[Observation.Id]] =
    generatePickler[SequencesQueue[Observation.Id]]

  implicit val sequenceQueueViewPickler: Pickler[SequencesQueue[SequenceView]] =
    generatePickler[SequencesQueue[SequenceView]]

  implicit val comaPickler: Pickler[ComaOption] = enumeratedPickler[ComaOption]

  implicit val tipTiltSourcePickler: Pickler[TipTiltSource]   = enumeratedPickler[TipTiltSource]
  implicit val serverLogLevelPickler: Pickler[ServerLogLevel] = enumeratedPickler[ServerLogLevel]
  implicit val m1SourcePickler: Pickler[M1Source]             = enumeratedPickler[M1Source]

  implicit val mountGuidePickler: Pickler[MountGuideOption]              = enumeratedPickler[MountGuideOption]
  implicit val m1GuideOnPickler: Pickler[M1GuideConfig.M1GuideOn]        =
    generatePickler[M1GuideConfig.M1GuideOn]
  implicit val m1GuideOffPickler: Pickler[M1GuideConfig.M1GuideOff.type] =
    generatePickler[M1GuideConfig.M1GuideOff.type]
  implicit val m1GuideConfigPickler: CompositePickler[M1GuideConfig]     =
    compositePickler[M1GuideConfig]
      .addConcreteType[M1GuideConfig.M1GuideOn]
      .addConcreteType[M1GuideConfig.M1GuideOff.type]

  implicit val m2GuideOnPickler: Pickler[M2GuideConfig.M2GuideOn]        =
    generatePickler[M2GuideConfig.M2GuideOn]
  implicit val m2GuideOffPickler: Pickler[M2GuideConfig.M2GuideOff.type] =
    generatePickler[M2GuideConfig.M2GuideOff.type]
  implicit val m2GuideConfigPickler: CompositePickler[M2GuideConfig]     =
    compositePickler[M2GuideConfig]
      .addConcreteType[M2GuideConfig.M2GuideOn]
      .addConcreteType[M2GuideConfig.M2GuideOff.type]

  implicit val telescopeGuideconfigPickler: Pickler[TelescopeGuideConfig] =
    generatePickler[TelescopeGuideConfig]

  implicit val resourceConflictPickler: Pickler[Notification.ResourceConflict] =
    generatePickler[Notification.ResourceConflict]
  implicit val instrumentInUsePickler: Pickler[Notification.InstrumentInUse]   =
    generatePickler[Notification.InstrumentInUse]
  implicit val requestFailedPickler: Pickler[Notification.RequestFailed]       =
    generatePickler[Notification.RequestFailed]
  implicit val subsystemlBusyPickler: Pickler[Notification.SubsystemBusy]      =
    generatePickler[Notification.SubsystemBusy]
  implicit val notificatonPickler: Pickler[Notification]                       =
    compositePickler[Notification]
      .addConcreteType[Notification.ResourceConflict]
      .addConcreteType[Notification.InstrumentInUse]
      .addConcreteType[Notification.RequestFailed]
      .addConcreteType[Notification.SubsystemBusy]

  implicit val observationCheckOverride: Pickler[UserPrompt.ObsConditionsCheckOverride] =
    generatePickler[UserPrompt.ObsConditionsCheckOverride]
  implicit val targetCheckOverride: Pickler[UserPrompt.TargetCheckOverride]             =
    generatePickler[UserPrompt.TargetCheckOverride]
  implicit val seqCheck: CompositePickler[SeqCheck]                                     =
    compositePickler[SeqCheck]
      .addConcreteType[UserPrompt.TargetCheckOverride]
      .addConcreteType[UserPrompt.ObsConditionsCheckOverride]
  implicit val checksOverridePickler: Pickler[ChecksOverride]                           =
    generatePickler[UserPrompt.ChecksOverride]
  implicit val userPromptPickler: Pickler[UserPrompt]                                   =
    compositePickler[UserPrompt]
      .addConcreteType[ChecksOverride]

  implicit val connectionOpenEventPickler: Pickler[ConnectionOpenEvent]                 =
    generatePickler[ConnectionOpenEvent]
  implicit val sequenceStartPickler: Pickler[SequenceStart]                             = generatePickler[SequenceStart]
  implicit val stepExecutedPickler: Pickler[StepExecuted]                               = generatePickler[StepExecuted]
  implicit val fileIdStepExecutedPickler: Pickler[FileIdStepExecuted]                   =
    generatePickler[FileIdStepExecuted]
  implicit val sequenceCompletedPickler: Pickler[SequenceCompleted]                     =
    generatePickler[SequenceCompleted]
  implicit val sequenceLoadedPickler: Pickler[SequenceLoaded]                           = generatePickler[SequenceLoaded]
  implicit val sequenceUnloadedPickler: Pickler[SequenceUnloaded]                       =
    generatePickler[SequenceUnloaded]
  implicit val stepBreakpointChangedPickler: Pickler[StepBreakpointChanged]             =
    generatePickler[StepBreakpointChanged]
  implicit val operatorUpdatedPickler: Pickler[OperatorUpdated]                         = generatePickler[OperatorUpdated]
  implicit val observerUpdatedPickler: Pickler[ObserverUpdated]                         = generatePickler[ObserverUpdated]
  implicit val conditionsUpdatedPickler: Pickler[ConditionsUpdated]                     =
    generatePickler[ConditionsUpdated]
  implicit val loadSequenceUpdatedPickler: Pickler[LoadSequenceUpdated]                 =
    generatePickler[LoadSequenceUpdated]
  implicit val clearLoadedSequencesUpdatedPickler: Pickler[ClearLoadedSequencesUpdated] =
    generatePickler[ClearLoadedSequencesUpdated]
  implicit val stepSkipMarkChangedPickler: Pickler[StepSkipMarkChanged]                 =
    generatePickler[StepSkipMarkChanged]
  implicit val sequencePauseRequestedPickler: Pickler[SequencePauseRequested]           =
    generatePickler[SequencePauseRequested]
  implicit val sequencePauseCanceledPickler: Pickler[SequencePauseCanceled]             =
    generatePickler[SequencePauseCanceled]
  implicit val sequenceRefreshedPickler: Pickler[SequenceRefreshed]                     =
    generatePickler[SequenceRefreshed]
  implicit val actionStopRequestedPickler: Pickler[ActionStopRequested]                 =
    generatePickler[ActionStopRequested]
  implicit val sequenceStoppedPickler: Pickler[SequenceStopped]                         = generatePickler[SequenceStopped]
  implicit val sequenceAbortedPickler: Pickler[SequenceAborted]                         = generatePickler[SequenceAborted]
  implicit val sequenceUpdatedPickler: Pickler[SequenceUpdated]                         = generatePickler[SequenceUpdated]
  implicit val sequenceErrorPickler: Pickler[SequenceError]                             = generatePickler[SequenceError]
  implicit val sequencePausedPickler: Pickler[SequencePaused]                           = generatePickler[SequencePaused]
  implicit val exposurePausedPickler: Pickler[ExposurePaused]                           = generatePickler[ExposurePaused]
  implicit val serverLogMessagePickler: Pickler[ServerLogMessage]                       =
    generatePickler[ServerLogMessage]
  implicit val userNotificationPickler: Pickler[UserNotification]                       =
    generatePickler[UserNotification]
  implicit val userPromptNotPickler: Pickler[UserPromptNotification]                    =
    generatePickler[UserPromptNotification]
  implicit val guideConfigPickler: Pickler[GuideConfigUpdate]                           = generatePickler[GuideConfigUpdate]
  implicit val queueUpdatedPickler: Pickler[QueueUpdated]                               = generatePickler[QueueUpdated]
  implicit val observationStagePickler: Pickler[ObserveStage]                           = enumeratedPickler[ObserveStage]
  implicit val observationProgressPickler: Pickler[ObservationProgress]                 =
    generatePickler[ObservationProgress]
  implicit val nsobseProgressPickler: Pickler[NSObservationProgress]                    =
    generatePickler[NSObservationProgress]
  implicit val progressPickler: CompositePickler[Progress]                              = compositePickler[Progress]
    .addConcreteType[ObservationProgress]
    .addConcreteType[NSObservationProgress]
  implicit val obsProgressPickler: Pickler[ObservationProgressEvent]                    =
    generatePickler[ObservationProgressEvent]
  implicit val acProgressPickler: Pickler[AlignAndCalibEvent]                           = generatePickler[AlignAndCalibEvent]
  implicit val singleActionEventPickler: Pickler[SingleActionEvent]                     =
    generatePickler[SingleActionEvent]
  implicit val nullEventPickler: Pickler[events.NullEvent.type]                         = generatePickler[NullEvent.type]
  implicit val overridesUpdatedPickler: Pickler[OverridesUpdated]                       =
    generatePickler[OverridesUpdated]

  // Composite pickler for the seqexec event hierarchy
  implicit val eventsPickler: CompositePickler[SeqexecEvent] = compositePickler[SeqexecEvent]
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
    .addConcreteType[SequenceAborted]
    .addConcreteType[SequenceUpdated]
    .addConcreteType[SequenceError]
    .addConcreteType[SequencePaused]
    .addConcreteType[ExposurePaused]
    .addConcreteType[ServerLogMessage]
    .addConcreteType[UserNotification]
    .addConcreteType[UserPromptNotification]
    .addConcreteType[GuideConfigUpdate]
    .addConcreteType[QueueUpdated]
    .addConcreteType[ObservationProgressEvent]
    .addConcreteType[SingleActionEvent]
    .addConcreteType[AlignAndCalibEvent]
    .addConcreteType[OverridesUpdated]
    .addConcreteType[NullEvent.type]

  implicit val userLoginPickler: Pickler[UserLoginRequest] = generatePickler[UserLoginRequest]

}
