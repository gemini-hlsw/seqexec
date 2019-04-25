// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Arbitrary._
import seqexec.model.RunningStep
import seqexec.model.StepId

trait ArbRunningStep {

  implicit val arbRunningStep: Arbitrary[RunningStep] =
    Arbitrary {
      for {
        l <- arbitrary[StepId]
        i <- arbitrary[StepId]
      } yield RunningStep(l, i)
    }

  implicit val runningStepCogen: Cogen[RunningStep] =
    Cogen[(StepId, StepId)].contramap(x => (x.last, x.total))

}

object ArbRunningStep extends ArbRunningStep
