// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.arb._

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import monocle.law.discipline._


final class TelescopeConfigSpec extends CatsSuite {

  import ArbTelescopeConfig._
  import ArbOffset._

  checkAll("TelescopeConfig", OrderTests[TelescopeConfig].order)

  checkAll("TelescopeConfig.p", LensTests(TelescopeConfig.p))
  checkAll("TelescopeConfig.q", LensTests(TelescopeConfig.q))

}
