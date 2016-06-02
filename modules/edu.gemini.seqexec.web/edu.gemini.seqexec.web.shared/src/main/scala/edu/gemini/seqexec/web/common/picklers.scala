package edu.gemini.seqexec.web.common

import boopickle.Default._
import edu.gemini.seqexec.model._

object picklers {
  // Composite pickler for the seqexec event hierarchy
  // It is not strictly need but reduces the size of the js
  implicit val eventsPickler = compositePickler[SeqexecEvent]
      .addConcreteType[NullEvent.type]
      .addConcreteType[SeqexecConnectionOpenEvent]
      .addConcreteType[SeqexecConnectionCloseEvent.type]
      .addConcreteType[SeqexecConnectionError]
      .addConcreteType[SequenceStartEvent]
      .addConcreteType[SequenceCompletedEvent]
      .addConcreteType[StepExecutedEvent]
}