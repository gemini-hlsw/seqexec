// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite

/**
  * Tests SeqexecServer typeclasses
  */
final class SeqexecServerSpec extends CatsSuite {
  import SeqexecServerArbitraries._

  checkAll("Eq[ObserveCommand.Result]", EqTests[ObserveCommand.Result].eqv)
}
