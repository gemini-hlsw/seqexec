// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import boopickle.Default._
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.model.Model.SeqexecEvent._

import java.time.Instant

/**
  * Contains boopickle implicit picklers of model objects
  * Boopickle can auto derived encoders but it is preferred to make
  * them explicitly
  */
@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Throw", "org.wartremover.warts.OptionPartial"))
trait ModelBooPicklers {

  // Composite pickler for the seqexec event hierarchy
  // It is not strictly need but reduces the size of the js
  implicit val sequenceStatePickler = compositePickler[SequenceState]
    .addConcreteType[SequenceState.Completed.type]
    .addConcreteType[SequenceState.Running.type]
    .addConcreteType[SequenceState.Stopping.type]
    .addConcreteType[SequenceState.Error]
    .addConcreteType[SequenceState.Paused.type]
    .addConcreteType[SequenceState.Idle.type]

  implicit val actionStatusPickler = compositePickler[ActionStatus]
    .addConcreteType[ActionStatus.Pending.type]
    .addConcreteType[ActionStatus.Completed.type]
    .addConcreteType[ActionStatus.Running]

  implicit val stepStatePickler = compositePickler[StepState]
    .addConcreteType[StepState.Pending.type]
    .addConcreteType[StepState.Completed.type]
    .addConcreteType[StepState.Skipped.type]
    .addConcreteType[StepState.Error]
    .addConcreteType[StepState.Running.type]
    .addConcreteType[StepState.Paused.type]

  implicit val stepPickler = compositePickler[Step]
    .addConcreteType[StandardStep]

  implicit val stepConfigPickler = generatePickler[SequenceView]

  implicit val datePickler = transformPickler((t: Long) => Instant.ofEpochMilli(t))(_.toEpochMilli)

  implicit val serverLogLevelPickler = compositePickler[ServerLogLevel]
    .addConcreteType[ServerLogLevel.INFO.type]
    .addConcreteType[ServerLogLevel.WARN.type]
    .addConcreteType[ServerLogLevel.ERROR.type]

  implicit val sequenceQueueIdPickler = generatePickler[SequencesQueue[SequenceId]]

  // Composite pickler for the seqexec event hierarchy
  // It is not strictly need but reduces the size of the js
  implicit val eventsPickler = compositePickler[Model.SeqexecEvent]
    .addConcreteType[ConnectionOpenEvent]
    .addConcreteType[SequenceStart]
    .addConcreteType[StepExecuted]
    .addConcreteType[SequenceCompleted]
    .addConcreteType[SequenceLoaded]
    .addConcreteType[SequenceUnloaded]
    .addConcreteType[StepBreakpointChanged]
    .addConcreteType[StepSkipMarkChanged]
    .addConcreteType[SequencePauseRequested]
    .addConcreteType[SequenceUpdated]
    .addConcreteType[SequenceRefreshed]
    .addConcreteType[NewLogMessage]
    .addConcreteType[ServerLogMessage]
    .addConcreteType[NullEvent.type]
    .addConcreteType[ObserverUpdated]
    .addConcreteType[OperatorUpdated]
    .addConcreteType[ConditionsUpdated]
    .addConcreteType[ResourcesBusy]

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

  /**
    * In most cases http4s will use the limit of a byte buffer but not for websockets
    * This method trims the binary array to be sent on the WS channel
    */
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def trimmedArray(e: Model.SeqexecEvent): Array[Byte] = {
    val byteBuffer = Pickle.intoBytes(e)
    val bytes = new Array[Byte](byteBuffer.limit())
    byteBuffer.get(bytes, 0, byteBuffer.limit)
    bytes
  }
}
