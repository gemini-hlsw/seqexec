package edu.gemini.seqexec.web.common

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import LogMessage._

import boopickle.Default._

/**
  * Tests Serialization/Deserialization using BooPickle
  */
class BooPicklingSpec extends FlatSpec with Matchers with PropertyChecks with ArbitrariesWebCommon {
  "StepConfig" should "upickle/depickle" in {
    forAll { (a: StepConfig) =>
      Unpickle[StepConfig].fromBytes(Pickle.intoBytes(a)) shouldEqual a
    }
  }
  "Step" should "upickle/depickle" in {
    forAll { (a: Step) =>
      Unpickle[Step].fromBytes(Pickle.intoBytes(a)) shouldEqual a
    }
  }
  "Sequence" should "upickle/depickle" in {
    forAll { (id: String, a: List[Step], s: SequenceState, i: String) =>
      val sequence = Sequence(id, s, i, SequenceSteps(a), None)
      Unpickle[Sequence].fromBytes(Pickle.intoBytes(sequence)) shouldEqual sequence
    }
  }
  "SequexecQueue" should "upickle/depickle" in {
    forAll { (id: String, a: List[Step], s: SequenceState, i: String) =>
      val sequenceQueue = SeqexecQueue(List(Sequence(id, s, i, SequenceSteps(a), None)))
      Unpickle[SeqexecQueue].fromBytes(Pickle.intoBytes(sequenceQueue)) shouldEqual sequenceQueue
    }
  }
  "LogMessage" should "upickle/depickle" in {
    forAll { (a: LogMessage) =>
      Unpickle[LogMessage].fromBytes(Pickle.intoBytes(a)) shouldEqual a
    }
  }
}
