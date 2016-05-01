package edu.gemini.seqexec.web.common

import org.scalacheck.{Arbitrary, _}
import org.scalacheck.Arbitrary._

trait ArbitrariesWebCommon {
  implicit val arbStepConfig: Arbitrary[StepConfig] =
    Arbitrary {
      for {
        k <- arbitrary[String]
        v <- arbitrary[String]
      } yield StepConfig(k, v)
    }

  implicit val arbStepState: Arbitrary[StepState] =
    Arbitrary(Gen.oneOf[StepState](StepState.Done, StepState.Error, StepState.NotDone, StepState.Paused, StepState.Running))

  implicit val arbStep: Arbitrary[Step] =
    Arbitrary {
      for {
        i <- arbitrary[Int]
        s <- arbitrary[StepState]
        v <- arbitrary[List[StepConfig]]
        f <- arbitrary[Option[String]]
      } yield Step(i, s, v, f)
    }

  implicit val arbSequenceState: Arbitrary[SequenceState] =
    Arbitrary(Gen.oneOf[SequenceState](SequenceState.Completed, SequenceState.Error, SequenceState.NotRunning, SequenceState.Running))

  implicit val arbSequence: Arbitrary[Sequence] =
    Arbitrary {
      for {
        id <- arbitrary[String]
        st <- arbitrary[SequenceState]
        i  <- Gen.oneOf(Instrument.instruments.list.toList)
        v  <- arbitrary[List[Step]]
      } yield Sequence(id, st, i, SequenceSteps(v), None)
    }
}
