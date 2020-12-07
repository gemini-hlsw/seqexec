// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import cats.Eq
import cats.data.NonEmptyList
import lucuma.core.util.arb.ArbEnumerated._
import seqexec.model.{Observation, StepId, UserPrompt}
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary._
import seqexec.model.UserPrompt.{ChecksOverride, Discrepancy, ObsConditionsCheckOverride, SeqCheck, TargetCheckOverride}

trait ArbUserPrompt {
  import ArbObservationId._

  implicit def discrepancyArb[A: Arbitrary: Eq]: Arbitrary[Discrepancy[A]] = Arbitrary[Discrepancy[A]] {
    for {
      actual   <- arbitrary[A]
      required <- arbitrary[A]
    } yield Discrepancy[A](actual, required)
  }

  implicit def discrepancyCogen[A: Cogen: Eq]: Cogen[Discrepancy[A]] =
    Cogen[(A, A)].contramap(x => (x.actual, x.required))

  implicit val targetCheckOverrideArb = Arbitrary[TargetCheckOverride] {
    for {
      self <- arbitrary[Discrepancy[String]]
    } yield TargetCheckOverride(self)
  }

  implicit val targetCheckOverrideCogen: Cogen[TargetCheckOverride] =
    Cogen[Discrepancy[String]].contramap(x => x.self)

  implicit val obsConditionsCheckOverrideArb: Arbitrary[ObsConditionsCheckOverride] =
    Arbitrary[ObsConditionsCheckOverride] {
      for {
        i  <- Gen.choose(0, 3)
        cc <- if(i == 0) arbitrary[Discrepancy[String]].map(Some(_)) else arbitrary[Option[Discrepancy[String]]]
        iq <- if(i == 1) arbitrary[Discrepancy[String]].map(Some(_)) else arbitrary[Option[Discrepancy[String]]]
        sc <- if(i == 2) arbitrary[Discrepancy[String]].map(Some(_)) else arbitrary[Option[Discrepancy[String]]]
        wv <- if(i == 3) arbitrary[Discrepancy[String]].map(Some(_)) else arbitrary[Option[Discrepancy[String]]]
      } yield ObsConditionsCheckOverride(cc, iq, sc, wv)
    }

  implicit val obsConditionsCheckOverrideCogen: Cogen[ObsConditionsCheckOverride] =
    Cogen[(Option[Discrepancy[String]],
      Option[Discrepancy[String]],
      Option[Discrepancy[String]],
      Option[Discrepancy[String]]
    )].contramap(x => (x.cc, x.iq, x.sb, x.wv))

  implicit val seqCheckCogen: Cogen[SeqCheck] =
    Cogen[Either[TargetCheckOverride, ObsConditionsCheckOverride]]
      .contramap{
        case a: TargetCheckOverride        => Left(a)
        case b: ObsConditionsCheckOverride => Right(b)
      }

  implicit val nelSeqCheckCogen: Cogen[NonEmptyList[SeqCheck]] =
    Cogen[(SeqCheck, List[SeqCheck])].contramap(x => (x.head, x.tail))

  private val checksGen = for {
    b   <- arbitrary[Boolean]
    tc  <- arbitrary[TargetCheckOverride]
    tco <- arbitrary[Option[TargetCheckOverride]]
    oc  <- arbitrary[ObsConditionsCheckOverride]
    oco <- arbitrary[Option[ObsConditionsCheckOverride]]
  } yield if(b) NonEmptyList(tc, oco.toList) else NonEmptyList(oc, tco.toList)

  implicit val checksOverrideArb: Arbitrary[ChecksOverride] = Arbitrary[ChecksOverride] {
    for {
      sid  <- arbitrary[Observation.Id]
      stid <- arbitrary[StepId]
      chks <- checksGen
    } yield ChecksOverride(sid, stid, chks)
  }

  implicit val checksOverrideCogen: Cogen[ChecksOverride] =
    Cogen[(Observation.Id, StepId, NonEmptyList[SeqCheck])].contramap( x => (x.sid, x.stepId, x.checks))

  implicit val userPromptArb = Arbitrary[UserPrompt] {
    for {
      r <- arbitrary[ChecksOverride]
    } yield r
  }

  implicit val userPromptCogen: Cogen[UserPrompt] =
    Cogen[Option[ChecksOverride]]
      .contramap {
        case r: ChecksOverride => Some(r)
        case _                 => None
      }

}

object ArbUserPrompt extends ArbUserPrompt
