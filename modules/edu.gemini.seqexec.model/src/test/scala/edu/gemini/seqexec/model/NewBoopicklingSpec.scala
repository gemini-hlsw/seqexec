package edu.gemini.seqexec.model

import SharedModel._
import SharedModel.SeqexecEvent._
import boopickle.Default._
import org.scalacheck.Arbitrary
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

// Keep the arbitraries in a separate trait to improve caching
object SharedModelArbitraries {
  import org.scalacheck.Shapeless._

  implicit val udArb  = implicitly[Arbitrary[UserDetails]]
  implicit val svArb  = implicitly[Arbitrary[SequenceView]]
  implicit val coeArb = implicitly[Arbitrary[ConnectionOpenEvent]]
  implicit val sseArb = implicitly[Arbitrary[SequenceStart]]
  implicit val seeArb = implicitly[Arbitrary[StepExecuted]]
  implicit val sceArb = implicitly[Arbitrary[SequenceCompleted]]
  implicit val sleArb = implicitly[Arbitrary[SequenceLoaded]]
  implicit val sbeArb = implicitly[Arbitrary[StepBreakpointChanged]]
  implicit val smeArb = implicitly[Arbitrary[StepSkipMarkChanged]]
  implicit val speArb = implicitly[Arbitrary[SequencePauseRequested]]
  implicit val lmArb  = implicitly[Arbitrary[NewLogMessage]]
}

/**
  * Tests Serialization/Deserialization using BooPickle
  */
class NewBoopicklingSpec extends FlatSpec with Matchers with PropertyChecks with NewBooPicklers {
  import SharedModelArbitraries._

  def testPickleUnpickle[A](implicit pickler: Pickler[A], arb: Arbitrary[A]) = {
    forAll { (a: A) =>
      Unpickle[A].fromBytes(Pickle.intoBytes(a)) shouldEqual a
    }
  }

  "SharedModel" should "pickle/depickle" in {
    // model
    testPickleUnpickle[UserDetails]
    testPickleUnpickle[SequenceView]

    // events
    testPickleUnpickle[ConnectionOpenEvent]
    testPickleUnpickle[SequenceStart]
    testPickleUnpickle[StepExecuted]
    testPickleUnpickle[SequenceCompleted]
    testPickleUnpickle[SequenceLoaded]
    testPickleUnpickle[StepBreakpointChanged]
    testPickleUnpickle[StepSkipMarkChanged]
    testPickleUnpickle[SequencePauseRequested]
    testPickleUnpickle[NewLogMessage]
  }
}
