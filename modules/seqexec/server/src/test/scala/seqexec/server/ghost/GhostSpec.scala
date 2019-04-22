// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite

/**
  * Tests GHOST Config typeclasses
  */
final class GhostSpec extends CatsSuite with GhostArbitraries {
  checkAll("Eq[GHOSTConfig]", EqTests[GhostConfig].eqv)
}
