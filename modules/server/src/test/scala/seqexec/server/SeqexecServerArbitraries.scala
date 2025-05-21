// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect.IO
import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import seqexec.model.BatchCommandState
import seqexec.model.Conditions
import seqexec.model.Observation
import seqexec.model.Operator
import seqexec.model.SeqexecModelArbitraries._
import seqexec.model.arb.ArbObservationId._
import seqexec.model.enum.Instrument

trait SeqexecServerArbitraries {

  implicit val selectedCoGen: Cogen[Map[Instrument, Observation.Id]] =
    Cogen[List[(Instrument, Observation.Id)]].contramap(_.toList)
  implicit val engineStateArb: Arbitrary[EngineState[IO]]            = Arbitrary {
    for {
      q <- arbitrary[ExecutionQueues]
      s <- arbitrary[Map[Instrument, Observation.Id]]
      c <- arbitrary[Conditions]
      o <- arbitrary[Option[Operator]]
    } yield EngineState.default[IO].copy(queues = q, selected = s, conditions = c, operator = o)
  }

  implicit val executionQueueArb: Arbitrary[ExecutionQueue] = Arbitrary {
    for {
      n <- arbitrary[String]
      s <- arbitrary[BatchCommandState]
      q <- arbitrary[List[Observation.Id]]
    } yield ExecutionQueue(n, s, q)
  }

  implicit val executionQueueCogen: Cogen[ExecutionQueue] =
    Cogen[(String, BatchCommandState, List[Observation.Id])]
      .contramap(x => (x.name, x.cmdState, x.queue))

}
