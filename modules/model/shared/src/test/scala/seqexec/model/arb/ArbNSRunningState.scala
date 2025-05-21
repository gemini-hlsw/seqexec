// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import seqexec.model._
import seqexec.model.arb.ArbNSSubexposure._
import seqexec.model.enum._

trait ArbNSRunningState {
  implicit val nsRunningStateArb = Arbitrary[NSRunningState] {
    for {
      a <- arbitrary[NSAction]
      u <- arbitrary[NSSubexposure]
    } yield NSRunningState(a, u)
  }

  implicit val nsRunningStateCogen: Cogen[NSRunningState] =
    Cogen[(NSAction, NSSubexposure)].contramap { x =>
      (x.action, x.sub)
    }

}

object ArbNSRunningState extends ArbNSRunningState
