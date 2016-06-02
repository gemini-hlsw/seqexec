package edu.gemini.seqexec.model

import edu.gemini.seqexec.model.dhs.ObsId
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import boopickle.Default._

/**
  * Tests Serialization/Deserialization using uPickle
  */
class BooPicklingSpec extends FlatSpec with Matchers with PropertyChecks {

  "SeqexecConnectionOpenEvent" should "upickle/depickle" in {
    Unpickle[SeqexecConnectionOpenEvent].fromBytes(Pickle.intoBytes(SeqexecConnectionOpenEvent(None))) shouldEqual SeqexecConnectionOpenEvent(None)
    // Mark event as the root class to properly encode it
    val event: SeqexecEvent = SeqexecConnectionOpenEvent(None)
    Unpickle[SeqexecEvent].fromBytes(Pickle.intoBytes(event)) shouldEqual event
    val user = Some(UserDetails("telops", "Telops"))
    Unpickle[SeqexecConnectionOpenEvent].fromBytes(Pickle.intoBytes(SeqexecConnectionOpenEvent(user))) shouldEqual SeqexecConnectionOpenEvent(user)
    val event2: SeqexecEvent = SeqexecConnectionOpenEvent(user)
    Unpickle[SeqexecEvent].fromBytes(Pickle.intoBytes(event2)) shouldEqual event2
  }

  /*"SeqexecConnectionCloseEvent" should "upickle/depickle" in {
      read[SeqexecConnectionCloseEvent.type](write(SeqexecConnectionCloseEvent)) shouldEqual SeqexecConnectionCloseEvent
  }

  "SequenceStartEvent" should "upickle/depickle" in {
    forAll { (i: String) =>
      read[SequenceStartEvent](write(SequenceStartEvent(i))) shouldEqual SequenceStartEvent(i)
    }
  }

  "StepExecutedEvent" should "upickle/depickle" in {
    forAll { (i: String, count: Int, file: ObsId) =>
      val event = StepExecutedEvent(i, count, count, file)
      read[StepExecutedEvent](write(event)) shouldEqual event
    }
  }

  "SeqexecConnectionError" should "upickle/depickle" in {
    forAll { (i: String) =>
      read[SeqexecConnectionError](write(SeqexecConnectionError(i))) shouldEqual SeqexecConnectionError(i)
    }
  }*/

}
