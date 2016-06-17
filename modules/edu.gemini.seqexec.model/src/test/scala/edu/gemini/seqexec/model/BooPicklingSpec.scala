package edu.gemini.seqexec.model

import edu.gemini.seqexec.model.dhs.ObsId
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import boopickle.Default._

/**
  * Tests Serialization/Deserialization using uPickle
  */
class BooPicklingSpec extends FlatSpec with Matchers with PropertyChecks {

  "NullEvent" should "pickle/depickle" in {
    Unpickle[NullEvent.type].fromBytes(Pickle.intoBytes(NullEvent)) shouldEqual NullEvent
    val event: SeqexecEvent = NullEvent
    Unpickle[SeqexecEvent].fromBytes(Pickle.intoBytes(event)) shouldEqual event
  }

  "SeqexecConnectionOpenEvent" should "pickle/depickle" in {
    Unpickle[SeqexecConnectionOpenEvent].fromBytes(Pickle.intoBytes(SeqexecConnectionOpenEvent(None))) shouldEqual SeqexecConnectionOpenEvent(None)
    // Mark event as the root class to properly encode it
    val event: SeqexecEvent = SeqexecConnectionOpenEvent(None)
    Unpickle[SeqexecEvent].fromBytes(Pickle.intoBytes(event)) shouldEqual event
    val user = Some(UserDetails("telops", "Telops"))
    Unpickle[SeqexecConnectionOpenEvent].fromBytes(Pickle.intoBytes(SeqexecConnectionOpenEvent(user))) shouldEqual SeqexecConnectionOpenEvent(user)
    val event2: SeqexecEvent = SeqexecConnectionOpenEvent(user)
    Unpickle[SeqexecEvent].fromBytes(Pickle.intoBytes(event2)) shouldEqual event2
  }

  "SeqexecConnectionCloseEvent" should "pickle/depickle" in {
    Unpickle[SeqexecConnectionCloseEvent.type].fromBytes(Pickle.intoBytes(SeqexecConnectionCloseEvent)) shouldEqual SeqexecConnectionCloseEvent
    val event: SeqexecEvent = SeqexecConnectionCloseEvent
    Unpickle[SeqexecEvent].fromBytes(Pickle.intoBytes(event)) shouldEqual event
  }

  "SeqexecConnectionErrorEvent" should "pickle/depickle" in {
    forAll { (i: String) =>
      Unpickle[SeqexecConnectionError].fromBytes(Pickle.intoBytes(SeqexecConnectionError(i))) shouldEqual SeqexecConnectionError(i)
      val event: SeqexecEvent = SeqexecConnectionError(i)
      Unpickle[SeqexecEvent].fromBytes(Pickle.intoBytes(event)) shouldEqual event
    }
  }

  "SequenceStartEvent" should "upickle/depickle" in {
    forAll { (i: String) =>
      Unpickle[SequenceStartEvent].fromBytes(Pickle.intoBytes(SequenceStartEvent(i))) shouldEqual SequenceStartEvent(i)
      val event: SeqexecEvent = SequenceStartEvent(i)
      Unpickle[SeqexecEvent].fromBytes(Pickle.intoBytes(event)) shouldEqual event
    }
  }

  "SequenceCompletedEvent" should "upickle/depickle" in {
    forAll { (i: String) =>
      Unpickle[SequenceCompletedEvent].fromBytes(Pickle.intoBytes(SequenceCompletedEvent(i))) shouldEqual SequenceCompletedEvent(i)
      val event: SeqexecEvent = SequenceCompletedEvent(i)
      Unpickle[SeqexecEvent].fromBytes(Pickle.intoBytes(event)) shouldEqual event
    }
  }

  "StepExecutedEvent" should "upickle/depickle" in {
    forAll { (i: String, count: Int, file: ObsId) =>
      val event = StepExecutedEvent(i, count, count, file)
      Unpickle[StepExecutedEvent].fromBytes(Pickle.intoBytes(event)) shouldEqual event
      val event2: SeqexecEvent = StepExecutedEvent(i, count, count, file)
      Unpickle[SeqexecEvent].fromBytes(Pickle.intoBytes(event2)) shouldEqual event2
    }
  }

}
