// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import gem.arb.ArbEnumerated._
import seqexec.server.gmos.GmosController.Config._

/**
  * Tests Gmos Config typeclasses
  */
final class GmosSpec extends CatsSuite {
  checkAll("Eq[ShutterState]", EqTests[ShutterState].eqv)
  checkAll("Eq[Beam]", EqTests[Beam].eqv)
}
