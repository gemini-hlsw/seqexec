// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.tcs

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import edu.gemini.seqexec.server.tcs.TcsController._

/**
  * Tests TcsController typeclasses
  */
final class TcsControllerSpec extends CatsSuite {
  checkAll("Eq[Beam]", EqTests[Beam].eqv)
  checkAll("Eq[NodAndChop]", EqTests[NodChop].eqv)
  checkAll("Eq[OffsetX]", EqTests[OffsetX].eqv)
  checkAll("Eq[OffsetY]", EqTests[OffsetY].eqv)
  checkAll("Eq[FocalPlaneOffset]", EqTests[FocalPlaneOffset].eqv)
  checkAll("Eq[OffsetA]", EqTests[OffsetA].eqv)
  checkAll("Eq[OffsetB]", EqTests[OffsetB].eqv)
  checkAll("Eq[OffsetC]", EqTests[OffsetC].eqv)
  checkAll("Eq[BinaryOnOff]", EqTests[BinaryOnOff].eqv)
}
