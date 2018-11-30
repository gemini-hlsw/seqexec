// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq
import cats.effect.IO
import cats.tests.CatsSuite
import seqexec.model.enum.Instrument
import seqexec.engine
import SequenceGen._
import seqexec.server.SeqexecServerArbitraries._
import gem.arb.ArbObservation
import gem.Observation
import monocle.law.discipline.LensTests
import seqexec.engine.{Action, Actions, Result}

/**
  * Tests SeqexecServer Lenses
  */
final class SeqexecServerLensesSpec extends CatsSuite with ArbObservation {

  // I tried to go down the rabbit hole with the Eqs, but it is not worth it for what they are used.
  implicit val streamEq: Eq[fs2.Stream[IO, Result]] = Eq.fromUniversalEquals
  implicit val actStateEq: Eq[Action.State] = Eq.fromUniversalEquals
  implicit val actionEq: Eq[Action[IO]] = Eq.by(x => (x.kind, x.gen, x.state))
  implicit val steppEq: Eq[HeaderExtraData => List[Actions[IO]]] = Eq.fromUniversalEquals
  implicit val stepActionsGenEq: Eq[StepActionsGen] = Eq.by(x => (x.pre))
  implicit val pndstepgEq: Eq[PendingStepGen] = Eq.by(x => (x.id, x.config, x.resources, x
    .generator))
  implicit val skipstepgEq: Eq[SkippedStepGen] = Eq.by(x => (x.id, x.config))
  implicit val cmpstepgEq: Eq[CompletedStepGen] = Eq.by(x => (x.id, x.config, x.fileId))
  implicit val stepqEq: Eq[StepGen] = Eq.instance{
    case (a:PendingStepGen, b:PendingStepGen)     => a === b
    case (a:SkippedStepGen, b:SkippedStepGen)     => a === b
    case (a:CompletedStepGen, b:CompletedStepGen) => a === b
    case _                                        => false
  }
  implicit val seqgEq: Eq[SequenceGen] = Eq.by(x => (x.id, x.title, x.instrument, x.steps))
  implicit val obsseqEq: Eq[SequenceData] = Eq.by(x => (x.observer, x.seqGen))
  implicit val seqstateEq: Eq[engine.Sequence.State[IO]] = Eq.fromUniversalEquals
  implicit val stateEq: Eq[EngineState] = Eq.by(x =>
    (x.queues, x.selected, x.conditions, x.operator, x.sequences))

  checkAll("selected optional",
           LensTests(EngineState.instrumentLoadedL(Instrument.GPI)))

  private val seqId = Observation.Id.unsafeFromString("GS-2018B-Q-0-1")
  // Some sanity checks
  test("Support inserting new loaded sequences") {
    val base = EngineState.default.copy(
      selected =
        Map(Instrument.F2 -> Observation.Id.unsafeFromString("GS-2018B-Q-1-1")))
    EngineState
      .instrumentLoadedL(Instrument.GPI)
      .set(seqId.some)
      .apply(base) shouldEqual base.copy(
      selected = base.selected + (Instrument.GPI -> seqId))
  }
  test("Support replacing loaded sequences") {
    val base = EngineState.default.copy(
      selected =
        Map(Instrument.GPI -> Observation.Id.unsafeFromString("GS-2018B-Q-1-1"),
            Instrument.F2  -> Observation.Id.unsafeFromString("GS-2018B-Q-2-1")))
    EngineState
      .instrumentLoadedL(Instrument.GPI)
      .set(seqId.some)
      .apply(base) shouldEqual base.copy(
      selected = base.selected.updated(Instrument.GPI, seqId))
  }
}
