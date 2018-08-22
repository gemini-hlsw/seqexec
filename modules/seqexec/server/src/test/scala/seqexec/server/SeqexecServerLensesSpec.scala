// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.tests.CatsSuite
import seqexec.model.enum.Instrument
import gem.arb.ArbObservation
import gem.Observation

/**
  * Tests SeqexecServer Lenses
  */
final class SeqexecServerLensesSpec extends CatsSuite with ArbObservation {

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
