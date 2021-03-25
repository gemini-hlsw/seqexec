// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import edu.gemini.seqexec.server.tcs.{ BinaryOnOff, BinaryYesNo }

/**
 * Tests Tcs typeclasses
 */
final class TcsSpec extends CatsSuite with TcsArbitraries {
  checkAll("Eq[BinaryYesNo]", EqTests[BinaryYesNo].eqv)
  checkAll("Eq[BinaryOnOff]", EqTests[BinaryOnOff].eqv)
  checkAll("Eq[CRFollow]", EqTests[CRFollow].eqv)
}
