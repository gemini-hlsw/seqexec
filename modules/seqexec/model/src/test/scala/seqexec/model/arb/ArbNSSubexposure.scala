// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import gem.arb.ArbEnumerated._
import seqexec.model._
import seqexec.model.enum._

trait ArbNSSubexposure {
  implicit val nsSubexposureArb = Arbitrary[NSSubexposure] {
    for {
      t  <- arbitrary[Int]
      c  <- arbitrary[Int]
      i  <- arbitrary[Int]
      s  <- arbitrary[NodAndShuffleStage]
    } yield
      new NSSubexposure(t, c, i , s)
  }

  implicit val nsSubexposureCogen: Cogen[NSSubexposure] =
    Cogen[(
      Int,
      Int,
      Int,
      NodAndShuffleStage
    )].contramap(
      s =>
        (s.totalCycles, s.cycle, s.id, s.stage)
    )

}

object ArbNSSubexposure extends ArbNSSubexposure
