// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.arb._
import gem.util.InstantMicros

import cats.Eq
import cats.kernel.laws._
import cats.tests.CatsSuite

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class EphemerisSpec extends CatsSuite {
  import ArbCoordinates._
  import ArbEphemeris._
  import ArbTime._
  import Ephemeris.Element

  // Laws
  checkAll("Ephemeris", GroupLaws[Ephemeris].monoid)
  checkAll("Ephemeris", OrderLaws[Ephemeris].eqv)

  test("Ephemeris.eq.naturality") {
    forAll { (a: Ephemeris, b: Ephemeris) =>
      a.equals(b) shouldEqual Eq[Ephemeris].eqv(a, b)
    }
  }

  test("Ephemeris.get.exact") {
    forAll { (a: Ephemeris, e: Element) =>
      (a ++ Ephemeris(e)).get(e._1) shouldEqual Some(e._2)
    }
  }

  test("Ephemeris.get.interpolated") {
    forAll { (t1: InstantMicros, c1: Coordinates, c2: Coordinates, n: Int) =>
      val offset = (n % 100).abs
      val (t2, t3) = (t1.plusSeconds(offset.toLong), t1.plusSeconds(100))
      val e = Ephemeris(t1 -> c1, t3 -> c2)
      val c3 = e.get(t2).getOrElse(sys.error("failed"))
      val c4 = c1.interpolate(c2, offset.toDouble / 100.00)
      (c3 angularDistance c4).toMicroarcseconds should be <= 15L
    }
  }

}
