// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import gem.laws.discipline._
import gem.arb._
import monocle.law.discipline._

final class RadialVelocitySpec extends CatsSuite {
  import ArbRadialVelocity._

  // Laws
  checkAll("RadialVelocity.fromMetersPerSecond", PrismTests(RadialVelocity.fromMetersPerSecond))
  checkAll("RadialVelocity.fromKilometersPerSecond", FormatTests(RadialVelocity.fromKilometersPerSecond).format)

}