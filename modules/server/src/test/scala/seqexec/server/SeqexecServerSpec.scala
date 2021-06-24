// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import lucuma.core.util.arb.ArbEnumerated._

/**
 * Tests SeqexecServer typeclasses
 */
final class SeqexecServerSpec extends CatsSuite with SeqexecServerArbitraries {
  checkAll("Eq[EpicsHealth]", EqTests[EpicsHealth].eqv)
}
