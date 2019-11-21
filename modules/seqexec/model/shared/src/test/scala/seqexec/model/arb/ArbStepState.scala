// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Cogen
import seqexec.model.StepState

trait ArbStepState {
  implicit val stepStateArb = Arbitrary[StepState] {
    for {
      v1 <- Gen.oneOf(StepState.Pending,
                      StepState.Completed,
                      StepState.Aborted,
                      StepState.Skipped,
                      StepState.Running,
                      StepState.Paused)
      v2 <- Gen.alphaStr.map(StepState.Failed.apply)
      r  <- Gen.oneOf(v1, v2)
    } yield r
  }

  implicit val stepStateCogen: Cogen[StepState] =
    Cogen[String].contramap(_.productPrefix)

}

object ArbStepState extends ArbStepState
