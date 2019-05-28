// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite

/**
  * Tests GPI Config typeclasses
  */
final class GpiSpec extends CatsSuite with GpiArbitraries {
  checkAll("Eq[AOFlags]", EqTests[AOFlags].eqv)
  checkAll("Eq[ArtificialSources]", EqTests[ArtificialSources].eqv)
  checkAll("Eq[Shutters]", EqTests[Shutters].eqv)
  checkAll("Eq[NonStandardModeParams]", EqTests[NonStandardModeParams].eqv)
  checkAll("Eq[GPIConfig]", EqTests[RegularGpiConfig].eqv)
}
