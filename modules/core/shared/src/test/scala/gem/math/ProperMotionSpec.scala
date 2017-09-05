// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import gem.arb._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class ProperMotionSpec extends CatsSuite {
  import ArbProperMotion._

  test("ProperMotion.identity") {
    forAll { (pm: ProperMotion) =>
      val c1 = pm.baseCoordinates
      val c2 = pm.plusYears(0.0).baseCoordinates
      c1.angularDistance(c2).toMicroarcseconds should be <= 20L
    }
  }

}
