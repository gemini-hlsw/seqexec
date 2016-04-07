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
    forAll { (id: String, a: List[Step], s: SequenceState, i: String) =>
      val sequence = Sequence(id, s, i, SequenceSteps(a), None)
      read[Sequence](write(sequence)) shouldEqual sequence
    }
  }
}
