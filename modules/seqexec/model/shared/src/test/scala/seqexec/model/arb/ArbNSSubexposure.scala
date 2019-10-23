// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import gem.arb.ArbEnumerated._
import seqexec.model._
import seqexec.model.enum.NodAndShuffleStage._
import seqexec.model.enum._
import seqexec.model.GmosParameters._
import shapeless.tag

trait ArbNSSubexposure {
  implicit val nsSubexposureArb = Arbitrary[NSSubexposure] {
    for {
      t  <- Gen.posNum[Int].map(tag[NsCyclesI][Int])
      c  <- Gen.choose(0, t).map(tag[NsCyclesI][Int])
      i  <- Gen.choose(0, NsSequence.length - 1)
    } yield
      NSSubexposure(t, c, i).getOrElse(NSSubexposure.Zero)
  }

  implicit val nsSubexposureCogen: Cogen[NSSubexposure] =
    Cogen[(
      Int,
      Int,
      Int,
      NodAndShuffleStage
    )].contramap(
      s =>
        (s.totalCycles, s.cycle, s.stageIndex, s.stage)
    )

}

object ArbNSSubexposure extends ArbNSSubexposure
