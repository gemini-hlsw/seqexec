// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.boopickle

import boopickle.Default._
import gem.Observation
import java.time.Instant
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.events._

/**
  * Contains boopickle implicit picklers of model objects
  * Boopickle can auto derived encoders but it is preferred to make
  * them explicitly
  */
@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.PublicInference", "org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Throw", "org.wartremover.warts.OptionPartial"))
trait ModelBooPicklers extends GemModelBooPicklers {
  //**********************
  // IMPORTANT The order of the picklers is very relevant to the generated size
  // add them with care
  //**********************
  implicit val instrumentPickler = generatePickler[Instrument]

  implicit val resourcePickler = generatePickler[Resource]

  implicit val operatorPickler = generatePickler[Operator]

  implicit val systemNamePickler = generatePickler[SystemName]

  implicit val observerPickler = generatePickler[Observer]

  implicit val userDetailsPickler = generatePickler[UserDetails]

  implicit val instantPickler = transformPickler((t: Long) => Instant.ofEpochMilli(t))(_.toEpochMilli)

  implicit val cloudCoverPickler = compositePickler[CloudCover]
    .addConcreteType[CloudCover.Any.type]
    .addConcreteType[CloudCover.Percent50.type]
    .addConcreteType[CloudCover.Percent70.type]
    .addConcreteType[CloudCover.Percent80.type]

  implicit val imageQualityPickler = compositePickler[ImageQuality]
    .addConcreteType[ImageQuality.Any.type]
    .addConcreteType[ImageQuality.Percent20.type]
    .addConcreteType[ImageQuality.Percent70.type]
    .addConcreteType[ImageQuality.Percent85.type]

  implicit val skyBackgroundPickler = compositePickler[SkyBackground]
    .addConcreteType[SkyBackground.Any.type]
    .addConcreteType[SkyBackground.Percent20.type]
    .addConcreteType[SkyBackground.Percent50.type]
    .addConcreteType[SkyBackground.Percent80.type]

  implicit val waterVaporPickler = compositePickler[WaterVapor]
    .addConcreteType[WaterVapor.Any.type]
    .addConcreteType[WaterVapor.Percent20.type]
    .addConcreteType[WaterVapor.Percent50.type]
    .addConcreteType[WaterVapor.Percent80.type]

  // Composite pickler for the seqexec event hierarchy
  // It is not strictly need but reduces the size of the js
  implicit val sequenceStatePickler = compositePickler[SequenceState]
    .addConcreteType[SequenceState.Completed.type]
    .addConcreteType[SequenceState.Running]
    .addConcreteType[SequenceState.Failed]
    .addConcreteType[SequenceState.Stopped.type]
    .addConcreteType[SequenceState.Idle.type]

  implicit val actionStatusPickler = compositePickler[ActionStatus]
    .addConcreteType[ActionStatus.Pending.type]
    .addConcreteType[ActionStatus.Completed.type]
    .addConcreteType[ActionStatus.Running.type]
    .addConcreteType[ActionStatus.Paused.type]
    .addConcreteType[ActionStatus.Failed.type]

  implicit val stepStatePickler = compositePickler[StepState]
    .addConcreteType[StepState.Pending.type]
    .addConcreteType[StepState.Completed.type]
    .addConcreteType[StepState.Skipped.type]
    .addConcreteType[StepState.Failed]
    .addConcreteType[StepState.Running.type]
    .addConcreteType[StepState.Paused.type]

  implicit val stepPickler = compositePickler[Step]
    .addConcreteType[StandardStep]

  implicit val sequenceMetadataPickler = generatePickler[SequenceMetadata]

  implicit val stepConfigPickler = generatePickler[SequenceView]

  implicit val serverLogLevelPickler = compositePickler[ServerLogLevel]
    .addConcreteType[ServerLogLevel.INFO.type]
    .addConcreteType[ServerLogLevel.WARN.type]
    .addConcreteType[ServerLogLevel.ERROR.type]

  implicit val sequenceQueueIdPickler = generatePickler[SequencesQueue[Observation.Id]]

  implicit val sequenceQueueViewPickler = generatePickler[SequencesQueue[SequenceView]]

  // Composite pickler for the seqexec event hierarchy
  // It is not strictly need but reduces the size of the js
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
    .addConcreteType[ClearLoadedSequencesUpdated.type]
    .addConcreteType[StepSkipMarkChanged]
    .addConcreteType[SequencePauseRequested]
    .addConcreteType[SequencePauseCanceled]
    .addConcreteType[SequenceRefreshed]
    .addConcreteType[ActionStopRequested]
    .addConcreteType[SequenceUpdated]
    .addConcreteType[SequenceError]
    .addConcreteType[SequencePaused]
    .addConcreteType[ExposurePaused]
    .addConcreteType[ResourcesBusy]
    .addConcreteType[ServerLogMessage]
    .addConcreteType[NullEvent.type]

}
