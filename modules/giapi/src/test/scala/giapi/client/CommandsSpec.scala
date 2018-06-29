// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.commands

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import giapi.client.GiapiArbitraries

/**
  * Tests Command typeclasses
  */
final class CommandSpec extends CatsSuite with GiapiArbitraries {
  checkAll("Eq[Configuration]", EqTests[Configuration].eqv)
  checkAll("Monoid[Configuration]", MonoidTests[Configuration].monoid)
}
