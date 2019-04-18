// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite

/**
  * Tests GSAOI Config typeclasses
  */
final class GsaoiSpec extends CatsSuite with GsaoiArbitraries {

  checkAll("Eq[WindowCover]", EqTests[WindowCover].eqv)
  checkAll("Eq[GsaoiController.DCConfig]",
           EqTests[GsaoiController.DCConfig].eqv)
  checkAll("Eq[GsaoiController.CCConfig]",
           EqTests[GsaoiController.CCConfig].eqv)
}
