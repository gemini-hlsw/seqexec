// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.arb.{ ArbEphemeris, ArbTimestamp }
import gem.util.Timestamp

import cats.Eq
import cats.kernel.laws.discipline._
import gsp.math.laws.discipline._
import cats.tests.CatsSuite

final class EphemerisSpec extends CatsSuite {
  import ArbEphemeris._
  import ArbTimestamp._
  import Ephemeris.Element

  // Laws
  checkAll("Ephemeris", MonoidTests[Ephemeris].monoid)
  checkAll("Ephemeris", EqTests[Ephemeris].eqv)
  checkAll("Ephemeris.elements", SplitMonoTests(Ephemeris.elements).splitMono)

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
    forAll { (t1: Timestamp, c1: EphemerisCoordinates, c2: EphemerisCoordinates, n: Int) =>
      val tEnd = t1.plusSeconds(100).getOrElse(Timestamp.Max)
      val tBeg = tEnd.plusSeconds(-100).get
      val off  = (n % 100).abs
      val tMid = tBeg.plusSeconds(off.toLong).get

      val e  = Ephemeris(tBeg -> c1, tEnd -> c2)
      val c3 = e.get(tMid).getOrElse(sys.error("failed"))
      val c4 = c1.interpolate(c2, off.toDouble / 100.00)
      (c3.coord angularDistance c4.coord).toMicroarcseconds should be <= 15L
    }
  }

}
