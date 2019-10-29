// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import gem.Observation
import gem.arb.ArbObservation._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen
import seqexec.model.arb.ArbTime._
import seqexec.model.arb.ArbNSSubexposure._
import seqexec.model._
import squants.time._

trait ArbObservationProgress {
  implicit val arbObservationProgress: Arbitrary[ObservationProgress] =
    Arbitrary {
      for {
        o <- arbitrary[Observation.Id]
        s <- arbitrary[StepId]
        t <- arbitrary[Time]
        r <- arbitrary[Time]
      } yield ObservationProgress(o, s, t, r)
    }

  implicit val observationInProgressCogen: Cogen[ObservationProgress] =
    Cogen[(Observation.Id, StepId, Time, Time)]
      .contramap(x => (x.obsId, x.stepId, x.total, x.remaining))

  implicit val arbNSObservationProgress: Arbitrary[NSObservationProgress] =
    Arbitrary {
      for {
        o <- arbitrary[Observation.Id]
        s <- arbitrary[StepId]
        t <- arbitrary[Time]
        r <- arbitrary[Time]
        u <- arbitrary[NSSubexposure]
      } yield NSObservationProgress(o, s, t, r, u)
    }

  implicit val nsObservationInProgressCogen: Cogen[NSObservationProgress] =
    Cogen[(Observation.Id, StepId, Time, Time, NSSubexposure)]
      .contramap(x => (x.obsId, x.stepId, x.total, x.remaining, x.sub))

  implicit val arbProgress: Arbitrary[Progress] =
    Arbitrary {
      for {
        o <- arbitrary[ObservationProgress]
        n <- arbitrary[NSObservationProgress]
        p <- Gen.oneOf(o, n)
      } yield  p
    }

  implicit val progressCogen: Cogen[Progress] =
    Cogen[Either[ObservationProgress, NSObservationProgress]]
      .contramap {
        case x: ObservationProgress   => Left(x)
        case x: NSObservationProgress => Right(x)
      }

}

object ArbObservationProgress extends ArbObservationProgress
