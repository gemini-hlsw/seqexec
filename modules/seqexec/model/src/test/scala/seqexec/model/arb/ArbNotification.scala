// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import gem.Observation
import gem.arb.ArbEnumerated._
import gem.arb.ArbObservation
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Cogen
import seqexec.model.StepId
import seqexec.model.Notification
import seqexec.model.ResourceConflict
import seqexec.model.RequestFailed
import seqexec.model.InstrumentInUse
import seqexec.model.SubsystemBusy
import seqexec.model.enum.Instrument
import seqexec.model.enum.Resource

trait ArbNotification extends ArbObservation {
  implicit val rcArb = Arbitrary[ResourceConflict] {
    for {
      id <- arbitrary[Observation.Id]
    } yield ResourceConflict(id)
  }

  implicit val rcCogen: Cogen[ResourceConflict] =
    Cogen[Observation.Id].contramap(_.sid)

  implicit val rfArb = Arbitrary[RequestFailed] {
    arbitrary[List[String]].map(RequestFailed.apply)
  }

  implicit val rfCogen: Cogen[RequestFailed] =
    Cogen[List[String]].contramap(_.msgs)

  implicit val inArb = Arbitrary[InstrumentInUse] {
    for {
      id <- arbitrary[Observation.Id]
      i  <- arbitrary[Instrument]
    } yield InstrumentInUse(id, i)
  }

  implicit val inCogen: Cogen[InstrumentInUse] =
    Cogen[(Observation.Id, Instrument)].contramap(x => (x.sid, x.ins))

  implicit val subsArb = Arbitrary[SubsystemBusy] {
    for {
      id <- arbitrary[Observation.Id]
      i  <- arbitrary[StepId]
      r  <- arbitrary[Resource]
    } yield SubsystemBusy(id, i, r)
  }

  implicit val subsCogen: Cogen[SubsystemBusy] =
    Cogen[(Observation.Id, StepId, Resource)].contramap(x => (x.oid, x.stepId, x.resource))

  implicit val notArb = Arbitrary[Notification] {
    for {
      r <- arbitrary[ResourceConflict]
      a <- arbitrary[InstrumentInUse]
      f <- arbitrary[RequestFailed]
      b <- arbitrary[SubsystemBusy]
      s <- Gen.oneOf(r, a, f, b)
    } yield s
  }

  implicit val notCogen: Cogen[Notification] =
    Cogen[Either[ResourceConflict, Either[InstrumentInUse, Either[RequestFailed, SubsystemBusy]]]]
      .contramap {
        case r: ResourceConflict => Left(r)
        case i: InstrumentInUse  => Right(Left(i))
        case f: RequestFailed    => Right(Right(Left(f)))
        case b: SubsystemBusy    => Right(Right(Right(b)))
      }

}

object ArbNotification extends ArbNotification
