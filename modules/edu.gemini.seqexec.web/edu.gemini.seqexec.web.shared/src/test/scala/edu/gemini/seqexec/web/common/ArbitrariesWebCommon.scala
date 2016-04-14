package edu.gemini.seqexec.web.common

import org.scalacheck.{Arbitrary, _}
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

trait ArbitrariesWebCommon {
  implicit val arbStepConfig: Arbitrary[StepConfig] =
    Arbitrary {
      for {
        k <- arbitrary[String]
        v <- arbitrary[String]
      } yield StepConfig(k, v)
    }

  implicit val arbStep: Arbitrary[Step] =
    Arbitrary {
      for {
        i <- arbitrary[Int]
        v <- arbitrary[List[StepConfig]]
      } yield Step(i, v)
    }

  implicit val arbSequenceState: Arbitrary[SequenceState] =
    Arbitrary(Gen.oneOf[SequenceState](SequenceState.Completed, SequenceState.Error, SequenceState.NotRunning, SequenceState.Running))

  implicit val arbSequence: Arbitrary[Sequence] =
    Arbitrary {
      for {
        id <- arbitrary[String]
        st <- arbitrary[SequenceState]
        i  <- Gen.oneOf(Instrument.instruments.list)
        v  <- arbitrary[List[StepConfig]]
      } yield Sequence(id, st, i, SequenceSteps(List(Step(0, v))), None)
    }
}
