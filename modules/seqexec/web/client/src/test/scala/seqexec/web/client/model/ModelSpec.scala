// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import seqexec.web.client.components.sequence.steps.OffsetFns.OffsetsDisplay
import seqexec.web.client.model._
import seqexec.web.client.circuit._
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
}
