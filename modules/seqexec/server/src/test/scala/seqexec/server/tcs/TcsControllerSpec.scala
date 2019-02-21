// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import seqexec.server.tcs.TcsController._

/**
  * Tests TcsController typeclasses
  */
final class TcsControllerSpec extends CatsSuite {

  import seqexec.server.SeqexecServerArbitraries._

  checkAll("Eq[Beam]", EqTests[Beam].eqv)
  checkAll("Eq[NodAndChop]", EqTests[NodChop].eqv)
  checkAll("Eq[InstrumentOffset]", EqTests[InstrumentOffset].eqv)
}
