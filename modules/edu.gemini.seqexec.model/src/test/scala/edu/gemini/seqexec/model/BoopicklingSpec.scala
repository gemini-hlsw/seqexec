package edu.gemini.seqexec.model

import Model._
import Model.SeqexecEvent._
import boopickle.Default._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalatest.{Assertion, FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

// Keep the arbitraries in a separate trait to improve caching
object SharedModelArbitraries {
  import org.scalacheck.Shapeless._
  val maxListSize = 5

  // N.B. We don't want to auto derive this to limit the size of the lists for performance reasons
  def sequencesQueueArb[A](implicit arb: Arbitrary[A]): Arbitrary[SequencesQueue[A]] = Arbitrary {
    for {
      a <- Gen.listOfN[A](maxListSize, arb.arbitrary)
    } yield SequencesQueue(a)
  }

  implicit val udArb  = implicitly[Arbitrary[UserDetails]]
  implicit val svArb  = implicitly[Arbitrary[SequenceView]]
  // Must define these early on to be used on the events
  implicit val sqiArb = sequencesQueueArb[SequenceId]
  implicit val sqvArb = sequencesQueueArb[SequenceView]
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
class BoopicklingSpec extends FlatSpec with Matchers with PropertyChecks with ModelBooPicklers {
  import SharedModelArbitraries._

  def testPickleUnpickle[A](implicit pickler: Pickler[A], arb: Arbitrary[A]): Assertion = {
    forAll { (a: A) =>
      Unpickle[A].fromBytes(Pickle.intoBytes(a)) shouldEqual a
    }
  }

  "SharedModel" should
    "pickle/depickle user details" in {
      // model
      testPickleUnpickle[UserDetails]
    }
    it should "pickle/depickle SequenceView" in {
      // model
      testPickleUnpickle[SequenceView]
    }
    it should "pickle/depickle ConnectionOpenEvent" in {
      // events
      testPickleUnpickle[ConnectionOpenEvent]
    }
    it should "pickle/depickle SequenceStart" in {
      // events
      testPickleUnpickle[SequencesQueue[SequenceView]]
    }
    it should "pickle/depickle StepExecuted" in {
      // events
      testPickleUnpickle[StepExecuted]
    }
    it should "pickle/depickle SequenceCompleted" in {
      // events
      testPickleUnpickle[SequenceCompleted]
    }
    it should "pickle/depickle SequenceLoaded" in {
      // events
      testPickleUnpickle[SequenceLoaded]
    }
    it should "pickle/depickle StepBreakpointChanged" in {
      // events
      testPickleUnpickle[StepBreakpointChanged]
    }
    it should "pickle/depickle StepSkipMarkChanged" in {
      // events
      testPickleUnpickle[StepSkipMarkChanged]
    }
    it should "pickle/depickle SequencePauseRequested" in {
      // events
      testPickleUnpickle[SequencePauseRequested]
    }
    it should "pickle/depickle NewLogMessage" in {
      // events
      testPickleUnpickle[NewLogMessage]
    }
    it should "pickle/depickle SequencesQueue[SequenceId]" in {
      // events
      testPickleUnpickle[SequencesQueue[SequenceId]]
    }
}
