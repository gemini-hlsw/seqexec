// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.tcs

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite

/**
  * Tests Tcs typeclasses
  */
final class TcsSpec extends CatsSuite {
  checkAll("Eq[BinaryYesNo]", EqTests[BinaryYesNo].eqv)
  checkAll("Eq[BinaryOnOff]", EqTests[BinaryOnOff].eqv)
}
