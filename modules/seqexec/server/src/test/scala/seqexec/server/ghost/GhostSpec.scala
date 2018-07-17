// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import seqexec.server.ghost.GHOSTController._

/**
  * Tests GHOST Config typeclasses
  */
final class GhostSpec extends CatsSuite {
  import seqexec.server.SeqexecServerArbitraries._

  checkAll("Eq[GHOSTConfig]", EqTests[GHOSTConfig].eqv)
}