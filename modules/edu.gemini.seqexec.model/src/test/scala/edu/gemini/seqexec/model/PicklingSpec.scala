package edu.gemini.seqexec.model

import edu.gemini.seqexec.model.dhs.ObsId
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import upickle.default._

/**
  * Tests Serialization/Deserialization using uPickle
  */
class PicklingSpec extends FlatSpec with Matchers with PropertyChecks {
  "SeqexecConnectionOpenEvent" should "upickle/depickle" in {
      read[SeqexecConnectionOpenEvent.type](write(SeqexecConnectionOpenEvent)) shouldEqual SeqexecConnectionOpenEvent
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
}
