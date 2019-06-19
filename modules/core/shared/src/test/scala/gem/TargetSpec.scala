// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._

import gem.arb._

final class TargetSpec extends CatsSuite {
  import ArbTarget._

  // Laws
  checkAll("Target",     OrderTests[Target].order)
  checkAll("TargetName", OrderTests[Target](Target.TargetNameOrder).order)
}
