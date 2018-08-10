// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import monocle.law.discipline.LensTests
import seqexec.web.client.components.sequence.steps.OffsetFns.OffsetsDisplay
import seqexec.web.client.model._
import seqexec.web.client.circuit._
// import seqexec.model.SharedModelArbitraries._
import org.scalajs.dom.WebSocket
import diode.data._

/**
  * Tests Client typeclasses
  */
final class ModelSpec extends CatsSuite with ArbitrariesWebClient {

  checkAll("Eq[OffsetsDisplay]", EqTests[OffsetsDisplay].eqv)
  checkAll("Eq[WebSocket]", EqTests[WebSocket].eqv)
  checkAll("Eq[Pot[A]]", EqTests[Pot[Int]].eqv)
  checkAll("Eq[WebSocketConnection]", EqTests[WebSocketConnection].eqv)
  checkAll("Eq[ClientStatus]", EqTests[ClientStatus].eqv)
  checkAll("Eq[RunningStep]", EqTests[RunningStep].eqv)
  checkAll("Eq[AvailableTab]", EqTests[AvailableTab].eqv)
  checkAll("Eq[SequenceTabActive]", EqTests[SequenceTabActive].eqv)
  checkAll("Eq[InstrumentSequenceTab]", EqTests[InstrumentSequenceTab].eqv)
  checkAll("Eq[PreviewSequenceTab]", EqTests[PreviewSequenceTab].eqv)
  checkAll("Eq[SequenceTab]", EqTests[SequenceTab].eqv)

  // lenses
  checkAll("Lens[SequenceTab, Option[Int]]", LensTests(SequenceTab.stepConfigL))
  // checkAll("Lens[SequenceTab, Option[SequenceView]]", LensTests(SequenceTab.completedSequenceL))
}
