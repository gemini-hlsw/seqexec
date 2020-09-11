// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.boopickle

import java.time.Instant

import boopickle.Default.Pickler
import boopickle.Default.UUIDPickler
import boopickle.Default.booleanPickler
import boopickle.Default.compositePickler
import boopickle.Default.doublePickler
import boopickle.Default.generatePickler
import boopickle.Default.intPickler
import boopickle.Default.longPickler
import boopickle.Default.optionPickler
import boopickle.Default.stringPickler
import boopickle.Default.transformPickler
import boopickle.DefaultBasic.iterablePickler
import boopickle.DefaultBasic.mapPickler
import cats._
import cats.implicits._
import gem.Observation
import lucuma.core.util.Enumerated
import seqexec.model.GmosParameters._
import seqexec.model.NodAndShuffleStep.PendingObserveCmd
import seqexec.model.UserPrompt.TargetCheckOverride
import seqexec.model._
import seqexec.model.dhs._
import seqexec.model.enum._
import seqexec.model.events._
import shapeless.tag
import shapeless.tag.@@
import squants.time.TimeConversions._

/**
 * Contains boopickle implicit picklers of model objects
 * Boopickle can auto derive encoders but it is preferred to make
 * them explicitly
 */
trait ModelBooPicklers extends GemModelBooPicklers {
  def valuesMap[F[_]: Traverse, A, B](c: F[A], f: A => B): Map[B, A] =
    c.fproduct(f).map(_.swap).toList.toMap

  def oldSourceIndex[A: gem.util.Enumerated]: Map[Int, A] =
    gem.util.Enumerated[A].all.zipWithIndex.map(_.swap).toMap

  def sourceIndex[A: Enumerated]: Map[Int, A] =
    Enumerated[A].all.zipWithIndex.map(_.swap).toMap

  def oldValuesMapPickler[A: Eq: gem.util.Enumerated, B: Monoid: Pickler](
    valuesMap: Map[B, A]
  ): Pickler[A] =
    transformPickler(
      (t: B) =>
        valuesMap
          .get(t)
          .getOrElse(throw new RuntimeException(s"Failed to decode value"))
    )(t => valuesMap.find { case (_, v) => v === t }.foldMap(_._1))

  def valuesMapPickler[A: Eq: Enumerated, B: Monoid: Pickler](
    valuesMap: Map[B, A]
  ): Pickler[A] =
    transformPickler((t: B) =>
      valuesMap
        .get(t)
        .getOrElse(throw new RuntimeException(s"Failed to decode value"))
    )(t => valuesMap.find { case (_, v) => v === t }.foldMap(_._1))

  def oldEnumeratedPickler[A: Eq: gem.util.Enumerated]: Pickler[A] =
    oldValuesMapPickler[A, Int](oldSourceIndex[A])

  def enumeratedPickler[A: Eq: Enumerated]: Pickler[A] =
    valuesMapPickler[A, Int](sourceIndex[A])

  implicit val timeProgressPickler =
    transformPickler((t: Double) => t.milliseconds)(_.toMilliseconds)

  implicit val instrumentPickler = oldEnumeratedPickler[Instrument]
  implicit val resourcePickler   = oldEnumeratedPickler[Resource]

  implicit val operatorPickler = generatePickler[Operator]

  implicit val systemNamePickler = oldEnumeratedPickler[SystemName]

  implicit val observerPickler = generatePickler[Observer]

  implicit val userDetailsPickler = generatePickler[UserDetails]

  implicit val instantPickler       =
    transformPickler((t: Long) => Instant.ofEpochMilli(t))(_.toEpochMilli)

  implicit val cloudCoverPickler    = enumeratedPickler[CloudCover]
  implicit val imageQualityPickler  = enumeratedPickler[ImageQuality]
  implicit val skyBackgroundPickler = enumeratedPickler[SkyBackground]
  implicit val waterVaporPickler    = enumeratedPickler[WaterVapor]
  implicit val conditionsPickler    = generatePickler[Conditions]

  implicit val sequenceStateCompletedPickler =
    generatePickler[SequenceState.Completed.type]
  implicit val sequenceStateIdlePickler      =
    generatePickler[SequenceState.Idle.type]
  implicit val sequenceStateRunningPickler   =
    generatePickler[SequenceState.Running]
  implicit val sequenceStateFailedPickler    =
    generatePickler[SequenceState.Failed]
  implicit val sequenceStateAbortedPickler   =
    generatePickler[SequenceState.Aborted.type]

  implicit val sequenceStatePickler = compositePickler[SequenceState]
    .addConcreteType[SequenceState.Completed.type]
    .addConcreteType[SequenceState.Running]
    .addConcreteType[SequenceState.Failed]
    .addConcreteType[SequenceState.Aborted.type]
    .addConcreteType[SequenceState.Idle.type]

  implicit val actionStatusPickler = oldEnumeratedPickler[ActionStatus]

  implicit val stepStatePendingPickler   = generatePickler[StepState.Pending.type]
  implicit val stepStateCompletedPickler =
    generatePickler[StepState.Completed.type]
  implicit val stepStateSkippedPickler   = generatePickler[StepState.Skipped.type]
  implicit val stepStateFailedPickler    = generatePickler[StepState.Failed]
  implicit val stepStateRunningPickler   = generatePickler[StepState.Running.type]
  implicit val stepStatePausedPickler    = generatePickler[StepState.Paused.type]
  implicit val stepStateAbortedPickler   = generatePickler[StepState.Aborted.type]

  implicit val stepStatePickler = compositePickler[StepState]
    .addConcreteType[StepState.Pending.type]
    .addConcreteType[StepState.Completed.type]
    .addConcreteType[StepState.Skipped.type]
    .addConcreteType[StepState.Failed]
    .addConcreteType[StepState.Aborted.type]
    .addConcreteType[StepState.Running.type]
    .addConcreteType[StepState.Paused.type]

  implicit val imageIdPickler = transformPickler((s: String) => tag[ImageFileIdT](s))(identity)
  implicit val standardStepPickler = generatePickler[StandardStep]
  implicit def taggedIntPickler[A]: Pickler[Int @@ A] = transformPickler((s: Int) => tag[A](s))(identity)
  implicit val nsStagePickler = oldEnumeratedPickler[NodAndShuffleStage]
  implicit val nsActionPickler = oldEnumeratedPickler[NSAction]
  implicit val nsSubexposurePickler: Pickler[NSSubexposure] =
    transformPickler[NSSubexposure, (NsCycles, NsCycles, Int)] {
      case (t: NsCycles, c: NsCycles, i: Int) =>
        NSSubexposure
          .apply(t, c, i)
          .getOrElse(
            throw new RuntimeException("Failed to decode ns subexposure")
          )
    }((ns: NSSubexposure) => (ns.totalCycles, ns.cycle, ns.stageIndex))
  implicit val nsRunningStatePickler = generatePickler[NSRunningState]
  implicit val nsStatusPickler = generatePickler[NodAndShuffleStatus]
  implicit val nsPendObsCmdPickler: Pickler[PendingObserveCmd] = oldEnumeratedPickler[PendingObserveCmd]
  implicit val nsStepPickler = generatePickler[NodAndShuffleStep]

  implicit val stepPickler = compositePickler[Step]
    .addConcreteType[StandardStep]
    .addConcreteType[NodAndShuffleStep]

  implicit val sequenceMetadataPickler = generatePickler[SequenceMetadata]

  implicit val stepConfigPickler = generatePickler[SequenceView]
  implicit val clientIdPickler   = generatePickler[ClientId]

  implicit val queueIdPickler            = generatePickler[QueueId]
  implicit val queueOpMovedPickler       = generatePickler[QueueManipulationOp.Moved]
  implicit val queueOpStartedPickler     =
    generatePickler[QueueManipulationOp.Started]
  implicit val queueOpStoppedPickler     =
    generatePickler[QueueManipulationOp.Stopped]
  implicit val queueOpClearPickler       = generatePickler[QueueManipulationOp.Clear]
  implicit val queueOpAddedSeqsPickler   =
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
  implicit val singleActionOpPickler          = compositePickler[SingleActionOp]
    .addConcreteType[SingleActionOp.Started]
    .addConcreteType[SingleActionOp.Completed]
    .addConcreteType[SingleActionOp.Error]

  implicit val batchCommandStateIdlePickler =
    generatePickler[BatchCommandState.Idle.type]
  implicit val batchCommandStateRun         = generatePickler[BatchCommandState.Run]
  implicit val batchCommandStateStopPickler =
    generatePickler[BatchCommandState.Stop.type]

  implicit val batchCommandPickler = compositePickler[BatchCommandState]
    .addConcreteType[BatchCommandState.Idle.type]
    .addConcreteType[BatchCommandState.Run]
    .addConcreteType[BatchCommandState.Stop.type]

  implicit val batchExecStatePickler = oldEnumeratedPickler[BatchExecState]

  implicit val executionQueuePickler = generatePickler[ExecutionQueueView]

  implicit val sequenceQueueIdPickler =
    generatePickler[SequencesQueue[Observation.Id]]

  implicit val sequenceQueueViewPickler =
    generatePickler[SequencesQueue[SequenceView]]

  implicit val comaPickler = oldEnumeratedPickler[ComaOption]

  implicit val tipTiltSourcePickler  = enumeratedPickler[TipTiltSource]
  implicit val serverLogLevelPickler = enumeratedPickler[ServerLogLevel]
  implicit val m1SourcePickler       = enumeratedPickler[M1Source]

  implicit val mountGuidePickler = enumeratedPickler[MountGuideOption]
  implicit val m1GuideOnPickler  = generatePickler[M1GuideConfig.M1GuideOn]
  implicit val m1GuideOffPickler =
    generatePickler[M1GuideConfig.M1GuideOff.type]
  implicit val m1GuideConfigPickler = compositePickler[M1GuideConfig]
    .addConcreteType[M1GuideConfig.M1GuideOn]
    .addConcreteType[M1GuideConfig.M1GuideOff.type]

  implicit val m2GuideOnPickler     = generatePickler[M2GuideConfig.M2GuideOn]
  implicit val m2GuideOffPickler    =
    generatePickler[M2GuideConfig.M2GuideOff.type]
  implicit val m2GuideConfigPickler = compositePickler[M2GuideConfig]
    .addConcreteType[M2GuideConfig.M2GuideOn]
    .addConcreteType[M2GuideConfig.M2GuideOff.type]

  implicit val telescopeGuideconfigPickler =
    generatePickler[TelescopeGuideConfig]

  implicit val resourceConflictPickler                   = generatePickler[Notification.ResourceConflict]
  implicit val instrumentInUsePickler                    = generatePickler[Notification.InstrumentInUse]
  implicit val requestFailedPickler                      = generatePickler[Notification.RequestFailed]
  implicit val subsystemlBusyPickler                     = generatePickler[Notification.SubsystemBusy]
  implicit val notificatonPickler: Pickler[Notification] =
    compositePickler[Notification]
      .addConcreteType[Notification.ResourceConflict]
      .addConcreteType[Notification.InstrumentInUse]
      .addConcreteType[Notification.RequestFailed]
      .addConcreteType[Notification.SubsystemBusy]

  implicit val targetCheckOverridePickler             = generatePickler[UserPrompt.TargetCheckOverride]
  implicit val userPromptPickler: Pickler[UserPrompt] =
    compositePickler[UserPrompt]
      .addConcreteType[TargetCheckOverride]

  implicit val connectionOpenEventPickler         = generatePickler[ConnectionOpenEvent]
  implicit val sequenceStartPickler               = generatePickler[SequenceStart]
  implicit val stepExecutedPickler                = generatePickler[StepExecuted]
  implicit val fileIdStepExecutedPickler          = generatePickler[FileIdStepExecuted]
  implicit val sequenceCompletedPickler           = generatePickler[SequenceCompleted]
  implicit val sequenceLoadedPickler              = generatePickler[SequenceLoaded]
  implicit val sequenceUnloadedPickler            = generatePickler[SequenceUnloaded]
  implicit val stepBreakpointChangedPickler       =
    generatePickler[StepBreakpointChanged]
  implicit val operatorUpdatedPickler             = generatePickler[OperatorUpdated]
  implicit val observerUpdatedPickler             = generatePickler[ObserverUpdated]
  implicit val conditionsUpdatedPickler           = generatePickler[ConditionsUpdated]
  implicit val loadSequenceUpdatedPickler         = generatePickler[LoadSequenceUpdated]
  implicit val clearLoadedSequencesUpdatedPickler =
    generatePickler[ClearLoadedSequencesUpdated]
  implicit val stepSkipMarkChangedPickler         = generatePickler[StepSkipMarkChanged]
  implicit val sequencePauseRequestedPickler      =
    generatePickler[SequencePauseRequested]
  implicit val sequencePauseCanceledPickler       =
    generatePickler[SequencePauseCanceled]
  implicit val sequenceRefreshedPickler   = generatePickler[SequenceRefreshed]
  implicit val actionStopRequestedPickler = generatePickler[ActionStopRequested]
  implicit val sequenceStoppedPickler     = generatePickler[SequenceStopped]
  implicit val sequenceAbortedPickler     = generatePickler[SequenceAborted]
  implicit val sequenceUpdatedPickler     = generatePickler[SequenceUpdated]
  implicit val sequenceErrorPickler       = generatePickler[SequenceError]
  implicit val sequencePausedPickler      = generatePickler[SequencePaused]
  implicit val exposurePausedPickler      = generatePickler[ExposurePaused]
  implicit val serverLogMessagePickler    = generatePickler[ServerLogMessage]
  implicit val userNotificationPickler    = generatePickler[UserNotification]
  implicit val userPromptNotPickler       = generatePickler[UserPromptNotification]
  implicit val guideConfigPickler         = generatePickler[GuideConfigUpdate]
  implicit val queueUpdatedPickler        = generatePickler[QueueUpdated]
  implicit val observationStagePickler    = oldEnumeratedPickler[ObserveStage]
  implicit val observationProgressPickler = generatePickler[ObservationProgress]
  implicit val nsobseProgressPickler      = generatePickler[NSObservationProgress]
  implicit val progressPickler = compositePickler[Progress]
    .addConcreteType[ObservationProgress]
    .addConcreteType[NSObservationProgress]
  implicit val obsProgressPickler                 = generatePickler[ObservationProgressEvent]
  implicit val acProgressPickler                  = generatePickler[AlignAndCalibEvent]
  implicit val singleActionEventPickler           = generatePickler[SingleActionEvent]
  implicit val nullEventPickler                   = generatePickler[NullEvent.type]

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
    .addConcreteType[NullEvent.type]

  implicit val userLoginPickler = generatePickler[UserLoginRequest]

}
