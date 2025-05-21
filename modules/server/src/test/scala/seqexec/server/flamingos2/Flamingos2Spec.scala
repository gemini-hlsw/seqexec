// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.FPUnit
import seqexec.server._
import seqexec.server.flamingos2.Flamingos2Controller.FocalPlaneUnit

/**
 * Tests F2 typeclasses
 */
final class Flamingos2Spec extends CatsSuite with Flamingos2Arbitraries {
  checkAll("Eq[FPUnit]", EqTests[FPUnit].eqv)
  checkAll("Eq[FocalPlaneUnit]", EqTests[FocalPlaneUnit].eqv)
}
