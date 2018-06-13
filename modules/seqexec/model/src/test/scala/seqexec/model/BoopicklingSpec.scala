// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import Model._
import events._
import cats.tests.CatsSuite
import boopickle.Default._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatest.{Assertion, Matchers}

/**
  * Tests Serialization/Deserialization using BooPickle
  */
@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.Throw", "org.wartremover.warts.OptionPartial", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Equals"))
final class BoopicklingSpec extends CatsSuite with Matchers {
  import ModelBooPicklers._
  import SharedModelArbitraries._
  import SequenceEventsArbitraries._

  def testPickleUnpickle[A](implicit pickler: Pickler[A], arb: Arbitrary[A]): Assertion = {
    forAll { (a: A) =>
      Unpickle[A].fromBytes(Pickle.intoBytes(a)) shouldEqual a
    }
  }

  test("pickle/depickle user details") {
    // model
    testPickleUnpickle[UserDetails]
  }
  test("pickle/depickle SequenceView") {
    // model
    testPickleUnpickle[SequenceView]
  }
  test("pickle/depickle ConnectionOpenEvent") {
    // events
    testPickleUnpickle[ConnectionOpenEvent]
  }
  test("pickle/depickle SequenceStart") {
    // events
    testPickleUnpickle[SequencesQueue[SequenceView]]
  }
  test("pickle/depickle StepExecuted") {
    // events
    testPickleUnpickle[StepExecuted]
  }
  test("pickle/depickle SequenceCompleted") {
    // events
    testPickleUnpickle[SequenceCompleted]
  }
  test("pickle/depickle SequenceLoaded") {
    // events
    testPickleUnpickle[SequenceLoaded]
  }
  test("pickle/depickle SequenceUnloaded") {
    // events
    testPickleUnpickle[SequenceUnloaded]
  }
  test("pickle/depickle StepBreakpointChanged") {
    // events
    testPickleUnpickle[StepBreakpointChanged]
  }
  test("pickle/depickle StepSkipMarkChanged") {
    // events
    testPickleUnpickle[StepSkipMarkChanged]
  }
  test("pickle/depickle SequencePauseRequested") {
    // events
    testPickleUnpickle[SequencePauseRequested]
  }
  test("pickle/depickle SequencePauseCanceled") {
    // events
    testPickleUnpickle[SequencePauseCanceled]
  }
  test("pickle/depickle SelectedSequenceUpdated") {
    // events
    testPickleUnpickle[SelectedSequenceUpdated]
  }
  test("pickle/depickle ActionStopRequested") {
    // events
    testPickleUnpickle[ActionStopRequested]
  }
  test("pickle/depickle SequenceError") {
    // events
    testPickleUnpickle[SequenceError]
  }
  test("pickle/depickle SequencePaused") {
    // events
    testPickleUnpickle[SequencePaused]
  }
  test("pickle/depickle ExposurePaused") {
    // events
    testPickleUnpickle[ExposurePaused]
  }
  test("pickle/depickle SequencesQueue[SequenceId]") {
    // events
    testPickleUnpickle[SequencesQueue[SequenceId]]
  }
  test("pickle/depickle ImageQuality") {
    testPickleUnpickle[ImageQuality]
  }
  test("pickle/depickle WaterVapor") {
    testPickleUnpickle[WaterVapor]
  }
  test("pickle/depickle SkyBackground") {
    testPickleUnpickle[SkyBackground]
  }
  test("pickle/depickle CloudCover") {
    testPickleUnpickle[CloudCover]
  }
  test("pickle/depickle Conditions") {
    testPickleUnpickle[Conditions]
  }
}
