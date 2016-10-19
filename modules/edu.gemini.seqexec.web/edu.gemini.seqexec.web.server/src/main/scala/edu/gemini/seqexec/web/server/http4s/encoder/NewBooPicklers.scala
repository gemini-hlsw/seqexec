package edu.gemini.seqexec.web.server.http4s.encoder

import boopickle.Default._
import edu.gemini.seqexec.model.SharedModel._

/**
  * Contains http4s implicit encoders of model objects
  */
trait NewBooPicklers {

  // Composite pickler for the seqexec event hierarchy
  // It is not strictly need but reduces the size of the js
  implicit val sequenceStatePickler = compositePickler[SequenceState]
    .addConcreteType[SequenceState.Completed.type]
    .addConcreteType[SequenceState.Running.type]
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

  implicit val stepPickler = compositePickler[Step]
    .addConcreteType[StandardStep]

  implicit val sequenceViewEncoder = booEncoderOf[SequenceView]

  // Composite pickler for the seqexec event hierarchy
  // It is not strictly need but reduces the size of the js
  implicit val eventsPickler = compositePickler[SeqexecEvent]
    .addConcreteType[SeqexecEvent.SequenceStart]
    .addConcreteType[SeqexecEvent.StepExecuted]
    .addConcreteType[SeqexecEvent.SequenceCompleted]
    .addConcreteType[SeqexecEvent.StepBreakpointChanged]
    .addConcreteType[SeqexecEvent.StepSkipMarkChanged]
    .addConcreteType[SeqexecEvent.SequencePauseRequested]

  /**
    * In most cases http4s will use the limit of a byte buffer but not for websockets
    * This method trims the binary array to be sent on the WS channel
    */
  def newTrimmedArray(e: SeqexecEvent): Array[Byte] = {
    val byteBuffer = Pickle.intoBytes(e)
    val bytes = new Array[Byte](byteBuffer.limit())
    byteBuffer.get(bytes, 0, byteBuffer.limit)
    bytes
  }
}
