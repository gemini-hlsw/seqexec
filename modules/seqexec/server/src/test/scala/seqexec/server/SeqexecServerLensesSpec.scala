// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.tests.CatsSuite
import monocle.law.discipline.LensTests
import seqexec.model.enum.Instrument
import seqexec.model.SharedModelArbitraries._
import gem.arb.ArbObservation
import gem.Observation

/**
  * Tests SeqexecServer Lenses
  */
final class SeqexecServerLensesSpec extends CatsSuite with ArbObservation {
  import SeqexecServerArbitraries._

  checkAll("queues lens", LensTests(EngineMetadata.queues))
  checkAll("conditions lens", LensTests(EngineMetadata.conditions))
  checkAll("operator lens", LensTests(EngineMetadata.operator))
  checkAll("selected lens", LensTests(EngineMetadata.selected))
  checkAll("selected optional",
           LensTests(EngineMetadata.selectedML(Instrument.GPI)))

  private val seqId = Observation.Id.unsafeFromString("GS-2018B-Q-0-1")
  // Some sanity checks
  test("Support inserting new loaded sequences") {
    val base = EngineMetadata.default.copy(
      selected =
        Map(Instrument.F2 -> Observation.Id.unsafeFromString("GS-2018B-Q-1-1")))
    EngineMetadata
      .selectedML(Instrument.GPI)
      .set(seqId.some)
      .apply(base) shouldEqual base.copy(
      selected = base.selected + (Instrument.GPI -> seqId))
  }
  test("Support replacing loaded sequences") {
    val base = EngineMetadata.default.copy(
      selected =
        Map(Instrument.GPI -> Observation.Id.unsafeFromString("GS-2018B-Q-1-1"),
            Instrument.F2  -> Observation.Id.unsafeFromString("GS-2018B-Q-2-1")))
    EngineMetadata
      .selectedML(Instrument.GPI)
      .set(seqId.some)
      .apply(base) shouldEqual base.copy(
      selected = base.selected.updated(Instrument.GPI, seqId))
  }
}
