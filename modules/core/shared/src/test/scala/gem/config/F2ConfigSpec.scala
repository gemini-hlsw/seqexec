// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.arb._
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite


final class F2ConfigSpec extends CatsSuite {

  import ArbEnumerated._
  import ArbFlamingos2._

  checkAll("DynamicConfig.Flamingos2", EqTests[DynamicConfig.Flamingos2].eqv)

}
