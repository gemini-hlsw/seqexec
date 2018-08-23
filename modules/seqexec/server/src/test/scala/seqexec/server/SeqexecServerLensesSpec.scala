// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq
import cats.tests.CatsSuite
import seqexec.model.enum.Instrument
import seqexec.engine
import seqexec.server.SeqexecServerArbitraries._
import gem.arb.ArbObservation
import gem.Observation
import monocle.law.discipline.LensTests

/**
  * Tests SeqexecServer Lenses
  */
final class SeqexecServerLensesSpec extends CatsSuite with ArbObservation {

  implicit val steppEq: Eq[HeaderExtraData => engine.Step] = Eq.fromUniversalEquals
  implicit val stepgEq: Eq[SequenceGen.Step] = Eq.by(x => (x.id, x.config, x.resources, x.generator))
  implicit val seqgEq: Eq[SequenceGen] = Eq.by(x => (x.id, x.title, x.instrument, x.steps))
  implicit val obsseqEq: Eq[ObserverSequence] = Eq.by(x => (x.observer, x.seq))
  implicit val seqstateEq: Eq[engine.Sequence.State] = Eq.fromUniversalEquals
  implicit val execstateEq: Eq[engine.Engine.State] = Eq.by(x => x.sequences)
  implicit val stateEq: Eq[EngineState] = Eq.by(x => (x.queues, x.selected, x.conditions, x.operator, x.sequences, x.executionState))

  checkAll("selected optional",
           LensTests(EngineState.selectedML(Instrument.GPI)))

  private val seqId = Observation.Id.unsafeFromString("GS-2018B-Q-0-1")
  // Some sanity checks
  test("Support inserting new loaded sequences") {
    val base = EngineState.default.copy(
      selected =
        Map(Instrument.F2 -> Observation.Id.unsafeFromString("GS-2018B-Q-1-1")))
    EngineState
      .selectedML(Instrument.GPI)
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
      .selectedML(Instrument.GPI)
      .set(seqId.some)
      .apply(base) shouldEqual base.copy(
      selected = base.selected.updated(Instrument.GPI, seqId))
  }
}
