// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import Model._
import Model.SeqexecEvent._
import boopickle.Default._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatest.{Assertion, FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

/**
  * Tests Serialization/Deserialization using BooPickle
  */
@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.Throw", "org.wartremover.warts.OptionPartial", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Equals"))
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
    it should "pickle/depickle SequenceUnloaded" in {
      // events
      testPickleUnpickle[SequenceUnloaded]
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
    it should "pickle/depickle SequencePauseCanceled" in {
      // events
      testPickleUnpickle[SequencePauseCanceled]
    }
    it should "pickle/depickle NewLogMessage" in {
      // events
      testPickleUnpickle[NewLogMessage]
    }
    it should "pickle/depickle SequencesQueue[SequenceId]" in {
      // events
      testPickleUnpickle[SequencesQueue[SequenceId]]
    }
    it should "pickle/depickle ImageQuality" in {
      testPickleUnpickle[ImageQuality]
    }
    it should "pickle/depickle WaterVapor" in {
      testPickleUnpickle[WaterVapor]
    }
    it should "pickle/depickle SkyBackground" in {
      testPickleUnpickle[SkyBackground]
    }
    it should "pickle/depickle CloudCover" in {
      testPickleUnpickle[CloudCover]
    }
    it should "pickle/depickle Conditions" in {
      testPickleUnpickle[Conditions]
    }
}
