package edu.gemini.seqexec.model

import boopickle.Default._
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.model.Model.SeqexecEvent._

/**
  * Contains boopickle implicit picklers of model objects
  * Boopickle can auto derived encoders but it is preferred to make
  * them explicitly
  */
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

  implicit val sequenceQueueIdPickler = generatePickler[SequencesQueue[SequenceId]]

  // Composite pickler for the seqexec event hierarchy
  // It is not strictly need but reduces the size of the js
  implicit val eventsPickler = compositePickler[Model.SeqexecEvent]
    .addConcreteType[ConnectionOpenEvent]
    .addConcreteType[SequenceStart]
    .addConcreteType[StepExecuted]
    .addConcreteType[SequenceCompleted]
    .addConcreteType[SequenceLoaded]
    .addConcreteType[StepBreakpointChanged]
    .addConcreteType[StepSkipMarkChanged]
    .addConcreteType[SequencePauseRequested]
    .addConcreteType[SequenceUpdated]
    .addConcreteType[SequenceRefreshed]
    .addConcreteType[NewLogMessage]
    .addConcreteType[ObserverUpdated]
    .addConcreteType[OperatorUpdated]

  implicit val conditionsPickler = compositePickler[Model.Conditions]

  implicit val cloudCoverPickler = compositePickler[CloudCover]
    .addConcreteType[CloudCover.Percent50.type]
    .addConcreteType[CloudCover.Percent70.type]
    .addConcreteType[CloudCover.Percent80.type]
    .addConcreteType[CloudCover.Any.type]

  implicit val imageQualityPickler = compositePickler[ImageQuality]
    .addConcreteType[ImageQuality.Percent20.type]
    .addConcreteType[ImageQuality.Percent70.type]
    .addConcreteType[ImageQuality.Percent85.type]
    .addConcreteType[ImageQuality.Any.type]

  implicit val skyBackgroundPickler = compositePickler[SkyBackground]
    .addConcreteType[SkyBackground.Percent20.type]
    .addConcreteType[SkyBackground.Percent50.type]
    .addConcreteType[SkyBackground.Percent80.type]
    .addConcreteType[SkyBackground.Any.type]

  implicit val waterVaporPickler = compositePickler[WaterVapor]
    .addConcreteType[WaterVapor.Percent20.type]
    .addConcreteType[WaterVapor.Percent50.type]
    .addConcreteType[WaterVapor.Percent80.type]
    .addConcreteType[WaterVapor.Any.type]

  /**
    * In most cases http4s will use the limit of a byte buffer but not for websockets
    * This method trims the binary array to be sent on the WS channel
    */
  def trimmedArray(e: Model.SeqexecEvent): Array[Byte] = {
    val byteBuffer = Pickle.intoBytes(e)
    val bytes = new Array[Byte](byteBuffer.limit())
    byteBuffer.get(bytes, 0, byteBuffer.limit)
    bytes
  }
}
