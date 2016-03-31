package edu.gemini.seqexec.web.common

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import upickle.default._

/**
  * Tests Serialization/Deserialization using uPickle
  */
class PicklingSpec extends FlatSpec with Matchers with PropertyChecks with Arbitraries {
  "StepConfig" should "upickle/depickle" in {
    forAll { (a: StepConfig) =>
      read[StepConfig](write(a)) shouldEqual a
    }
  }
  "Step" should "upickle/depickle" in {
    forAll { (a: Step) =>
      read[Step](write(a)) shouldEqual a
    }
  }
  "Sequence" should "upickle/depickle" in {
    forAll { (i: String, a: List[Step]) =>
      val sequence = Sequence(i, SequenceSteps(a))
      read[Sequence](write(sequence)) shouldEqual sequence
    }
  }
  "SequenceInQueue" should "upickle/depickle" in {
    forAll { (i: String, s: SequenceState, inst: String, e: Option[String]) =>
      val sequenceInQueue = SequenceInQueue(i, s, inst, e)
      read[SequenceInQueue](write(sequenceInQueue)) shouldEqual sequenceInQueue
    }
  }
}
