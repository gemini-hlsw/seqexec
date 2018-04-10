// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.flamingos2

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import edu.gemini.seqexec.server.flamingos2.Flamingos2Controller.FocalPlaneUnit
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.FPUnit

/**
  * Tests F2 typeclasses
  */
final class Flamingos2Spec extends CatsSuite {
  checkAll("Eq[FPUnit]", EqTests[FPUnit].eqv)
  checkAll("Eq[FocalPlaneUnit]", EqTests[FocalPlaneUnit].eqv)
}
