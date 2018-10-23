// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.Eq
import cats.effect.IO
import cats.tests.CatsSuite
import gem.Observation
import gem.arb.ArbObservation
import monocle.law.discipline.OptionalTests
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Arbitrary._
import seqexec.model.SeqexecModelArbitraries._
import seqexec.model.SequenceState

final class EngineSpec extends CatsSuite with ArbObservation {
  implicit val seqstateEq: Eq[Sequence.State[IO]] = Eq.fromUniversalEquals
  implicit val execstateEq: Eq[Engine.State] = Eq.by(x => x.sequences)

  implicit val sequenceArb: Arbitrary[Sequence[IO]] = Arbitrary{
    for{
      id <- arbitrary[Observation.Id](ArbObservation.arbObservationId)
    } yield Sequence(id, List())
  }

  implicit val sequenceStateArb: Arbitrary[Sequence.State[IO]] = Arbitrary{
    for{
      seq <- arbitrary[Sequence[IO]]
      st <- arbitrary[SequenceState]
    } yield Sequence.State.Final(seq, st)
  }

  implicit val sequenceStateCogen: Cogen[Sequence.State[IO]] = Cogen[Observation.Id].contramap(_.toSequence.id)

  implicit val engineStateArb: Arbitrary[Engine.State] = Arbitrary {
    for {
      q <- arbitrary[Map[Observation.Id, Sequence.State[IO]]]
    } yield Engine.State(q)
  }

  checkAll("sequence optional",
           OptionalTests[Engine.State, Sequence.State[IO], Observation.Id](Engine.State.sequenceState))

}
