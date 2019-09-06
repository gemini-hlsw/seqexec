// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import gem.arb.ArbEnumerated._
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.arb.ArbStepConfig._
import seqexec.model.arb.ArbStepState._
import seqexec.model.arb.ArbDhsTypes._

trait ArbNodAndShuffleStep {
  implicit val nssArb = Arbitrary[NodAndShuffleStatus] {
    arbitrary[ActionStatus].map(NodAndShuffleStatus.apply)
  }

  implicit val nodAndShuffleStatusCogen: Cogen[NodAndShuffleStatus] =
    Cogen[ActionStatus].contramap {
      _.observing
    }

  implicit val nodShuffleStepArb = Arbitrary[NodAndShuffleStep] {
    for {
      id <- arbitrary[StepId]
      c  <- stepConfigGen
      s  <- arbitrary[StepState]
      b  <- arbitrary[Boolean]
      k  <- arbitrary[Boolean]
      f  <- arbitrary[Option[dhs.ImageFileId]]
      cs <- arbitrary[List[(Resource, ActionStatus)]]
      os <- arbitrary[NodAndShuffleStatus]
    } yield
      new NodAndShuffleStep(id           = id,
                            config       = c,
                            status       = s,
                            breakpoint   = b,
                            skip         = k,
                            fileId       = f,
                            configStatus = cs,
                            nsStatus     = os)
  }

  implicit val nodShuffleStepCogen: Cogen[NodAndShuffleStep] =
    Cogen[(
      StepId,
      Map[SystemName, Map[String, String]],
      StepState,
      Boolean,
      Boolean,
      Option[dhs.ImageFileId],
      List[(Resource, ActionStatus)],
      NodAndShuffleStatus
    )].contramap(
      s =>
        (s.id,
         s.config,
         s.status,
         s.breakpoint,
         s.skip,
         s.fileId,
         s.configStatus,
         s.nsStatus)
    )

}

object ArbNodAndShuffleStep extends ArbNodAndShuffleStep
