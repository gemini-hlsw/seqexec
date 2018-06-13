// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.tests.CatsSuite
import monocle.law.discipline.LensTests
import seqexec.model.Model.Instrument
import seqexec.model.SharedModelArbitraries._

/**
  * Tests SeqexecServer Lenses
  */
final class SeqexecServerLensesSpec extends CatsSuite {
  import SeqexecServerArbitraries._

  checkAll("queues lens", LensTests(EngineMetadata.queues))
  checkAll("conditions lens", LensTests(EngineMetadata.conditions))
  checkAll("operator lens", LensTests(EngineMetadata.operator))
  checkAll("selected lens", LensTests(EngineMetadata.selected))
  checkAll("selected optional", LensTests(EngineMetadata.selectedML(Instrument.GPI)))
  val seqId = "GS-2018-Q-0"
  // Some sanity checks
  test("Support inserting new loaded sequences") {
    val base = EngineMetadata.default.copy(selected = Map(Instrument.F2 -> "Test"))
    EngineMetadata.selectedML(Instrument.GPI).set("GS-2018-Q-0".some).apply(base) shouldEqual base.copy(selected = base.selected+ (Instrument.GPI -> seqId))
  }
  test("Support replacing loaded sequences") {
    val base = EngineMetadata.default.copy(selected = Map(Instrument.GPI -> "AnotherTest", Instrument.F2 -> "Test"))
    EngineMetadata.selectedML(Instrument.GPI).set("GS-2018-Q-0".some).apply(base) shouldEqual base.copy(selected = base.selected.updated(Instrument.GPI, seqId))
  }
}
