// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.arb._
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite


final class GnirsConfigSpec extends CatsSuite {

  import ArbEnumerated._
  import ArbGnirs._

  checkAll("DynamicConfig.Gnirs", EqTests[DynamicConfig.Gnirs].eqv)

}
