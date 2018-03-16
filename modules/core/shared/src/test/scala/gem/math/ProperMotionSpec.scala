// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import gem.arb._
import monocle.law.discipline._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class ProperMotionSpec extends CatsSuite {
  import ArbAngle._
  import ArbCoordinates._
  import ArbEpoch._
  import ArbOffset._
  import ArbProperMotion._
  import ArbRadialVelocity._

  // Laws
  checkAll("ProperMotion", OrderTests[ProperMotion].order)
  checkAll("ProperMotion.baseCoordinates", LensTests(ProperMotion.baseCoordinates))
  checkAll("ProperMotion.epoch", LensTests(ProperMotion.epoch))
  checkAll("ProperMotion.properVelocity", LensTests(ProperMotion.properVelocity))
  checkAll("ProperMotion.radialVelocity", LensTests(ProperMotion.radialVelocity))
  checkAll("ProperMotion.parallax", LensTests(ProperMotion.parallax))

  test("ProperMotion.identity") {
    forAll { (pm: ProperMotion) =>
      val c1 = pm.baseCoordinates
      val c2 = pm.plusYears(0.0).baseCoordinates
      c1.angularDistance(c2).toMicroarcseconds should be <= 20L
    }
  }

}
