// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq
import cats.tests.CatsSuite
import seqexec.model.enum.Instrument
import seqexec.engine
import SequenceGen._
import gem.arb.ArbObservation
import gem.Observation
import monocle.law.discipline.LensTests
import seqexec.engine.{Action, Actions}

/**
  * Tests SeqexecServer Lenses
  */
final class SeqexecServerLensesSpec extends CatsSuite with SeqexecServerArbitraries with ArbObservation {

  // I tried to go down the rabbit hole with the Eqs, but it is not worth it for what they are used.
  implicit def actStateEq[F[_]]: Eq[Action.State[F]] = Eq.fromUniversalEquals
  implicit def actionEq[F[_]]: Eq[Action[F]] = Eq.by(x => (x.kind, x.state))
  implicit def steppEq[F[_]]: Eq[HeaderExtraData => List[Actions[F]]] = Eq.fromUniversalEquals
  implicit def stepActionsGenEq[F[_]]: Eq[StepActionsGen[F]] = Eq.by(x => (x.pre, x.configs, x.post))
  implicit def pndstepgEq[F[_]]: Eq[PendingStepGen[F]] = Eq.by(x => (x.id, x.config, x.resources, x
    .generator))
  implicit val skipstepgEq: Eq[SkippedStepGen] = Eq.by(x => (x.id, x.config))
  implicit val cmpstepgEq: Eq[CompletedStepGen] = Eq.by(x => (x.id, x.config, x.fileId))
  implicit def stepqEq[F[_]]: Eq[StepGen[F]] = Eq.instance {
    case (a: PendingStepGen[F],   b: PendingStepGen[F]) => a === b
    case (a: SkippedStepGen,   b: SkippedStepGen)       => a === b
    case (a: CompletedStepGen, b: CompletedStepGen)     => a === b
    case _                                              => false
  }
  implicit def seqgEq[F[_]]: Eq[SequenceGen[F]] = Eq.by(x => (x.id, x.title, x.instrument, x.steps))
  implicit def obsseqEq[F[_]]: Eq[SequenceData[F]] = Eq.by(x => (x.observer, x.seqGen))
  implicit def seqstateEq[F[_]]: Eq[engine.Sequence.State[F]] = Eq.fromUniversalEquals
  implicit val stateEq: Eq[EngineState] = Eq.by(x =>
    (x.queues, x.selected, x.conditions, x.operator, x.sequences))

  checkAll("selected optional",
           LensTests(EngineState.instrumentLoadedL(Instrument.Gpi)))

  private val seqId = Observation.Id.unsafeFromString("GS-2018B-Q-0-1")
  // Some sanity checks
  test("Support inserting new loaded sequences") {
    val base = EngineState.default.copy(
      selected =
        Map(Instrument.F2 -> Observation.Id.unsafeFromString("GS-2018B-Q-1-1")))
    EngineState
      .instrumentLoadedL(Instrument.Gpi)
      .set(seqId.some)
      .apply(base) shouldEqual base.copy(
      selected = base.selected + (Instrument.Gpi -> seqId))
  }
  test("Support replacing loaded sequences") {
    val base = EngineState.default.copy(
      selected =
        Map(Instrument.Gpi -> Observation.Id.unsafeFromString("GS-2018B-Q-1-1"),
            Instrument.F2  -> Observation.Id.unsafeFromString("GS-2018B-Q-2-1")))
    EngineState
      .instrumentLoadedL(Instrument.Gpi)
      .set(seqId.some)
      .apply(base) shouldEqual base.copy(
      selected = base.selected.updated(Instrument.Gpi, seqId))
  }
}
