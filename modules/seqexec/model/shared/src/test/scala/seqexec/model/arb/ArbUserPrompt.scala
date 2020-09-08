// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import gem.Observation
import gem.arb.ArbEnumerated._
import gem.arb.ArbObservation
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import seqexec.model.UserPrompt
import seqexec.model.UserPrompt.TargetCheckOverride

trait ArbUserPrompt extends ArbObservation {
  implicit val targetChecOverrideArb = Arbitrary[TargetCheckOverride] {
    for {
      id <- arbitrary[Observation.Id]
    } yield TargetCheckOverride(id)
  }

  implicit val targetCheckOverrideCogen: Cogen[TargetCheckOverride] =
    Cogen[Observation.Id].contramap(_.sid)

  implicit val userPromptArb = Arbitrary[UserPrompt] {
    for {
      r <- arbitrary[TargetCheckOverride]
    } yield r
  }

  implicit val userPromptCogen: Cogen[UserPrompt] =
    Cogen[Option[TargetCheckOverride]]
      .contramap {
        case r: TargetCheckOverride => Some(r)
        case _ => None
      }

}

object ArbUserPrompt extends ArbUserPrompt
