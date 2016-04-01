package edu.gemini.seqexec.web.common

import org.scalacheck.{Arbitrary, _}
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

trait Arbitraries {
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

  implicit val arbSequenceInQueue: Arbitrary[SequenceInQueue] =
    Arbitrary {
      for {
        i  <- arbitrary[String]
        s  <- arbitrary[SequenceState]
        in <- arbitrary[String]
        e  <- arbitrary[Option[String]]
      } yield SequenceInQueue(i, s, in, e)
    }

}
