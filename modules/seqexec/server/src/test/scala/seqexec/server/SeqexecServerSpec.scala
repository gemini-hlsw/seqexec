// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import gem.arb.ArbEnumerated._

/**
  * Tests SeqexecServer typeclasses
  */
final class SeqexecServerSpec extends CatsSuite with SeqexecServerArbitraries {
  checkAll("Eq[EpicsHealth]", EqTests[EpicsHealth].eqv)
}
