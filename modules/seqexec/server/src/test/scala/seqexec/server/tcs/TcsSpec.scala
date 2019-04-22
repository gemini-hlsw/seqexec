// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo}
import monocle.law.discipline.PrismTests

/**
  * Tests Tcs typeclasses
  */
final class TcsSpec extends CatsSuite with TcsArbitraries {
  checkAll("Eq[BinaryYesNo]", EqTests[BinaryYesNo].eqv)
  checkAll("Eq[BinaryOnOff]", EqTests[BinaryOnOff].eqv)
  checkAll("Eq[CRFollow]", EqTests[CRFollow].eqv)
  checkAll("Prism[Int, CRFollow]", PrismTests(CRFollow.fromInt))
}
