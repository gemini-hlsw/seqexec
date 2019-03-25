// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import gem.arb._

final class TargetEnvironmentSpec extends CatsSuite {
  import ArbTargetEnvironment._
  checkAll(s"TargetEnvironment", EqTests[TargetEnvironment].eqv)
}
