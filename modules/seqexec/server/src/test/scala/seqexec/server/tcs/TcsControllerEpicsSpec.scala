// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite

/**
  * Tests TcsControllerEpics typeclasses
  */
final class TcsControllerEpicsSpec extends CatsSuite {
  import seqexec.server.SeqexecServerArbitraries._

  checkAll("Eq[SFInstName]", EqTests[TcsControllerEpics.CodexScienceFoldPosition.SFInstName].eqv)
}
