// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.Eq
import cats.implicits._
import gem.arb._
import java.time.Instant
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
class EphemerisSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbCoordinates._
  import ArbEphemeris._
  import ArbTime._
  import Ephemeris.Element

  "Equality" must "be natural" in {
    forAll { (a: Ephemeris, b: Ephemeris) =>
      a.equals(b) shouldEqual Eq[Ephemeris].eqv(a, b)
    }
  }

  "Monoid" must "have left identity" in {
    forAll { (a: Ephemeris) =>
      Ephemeris.Empty |+| a shouldEqual a
    }
  }

  it must "have right identity" in {
    forAll { (a: Ephemeris) =>
      a |+| Ephemeris.Empty shouldEqual a
    }
  }

  it must "be associative" in {
    forAll { (a: Ephemeris, b: Ephemeris, c: Ephemeris) =>
      (a |+| b) |+| c shouldEqual a |+| (b |+| c)
    }
  }

  "get" must "be exact when the point is known" in {
    forAll { (a: Ephemeris, e: Element) =>
      (a ++ Ephemeris(e)).get(e._1) shouldEqual Some(e._2)
    }
  }

  it must "interpolate when the point is unknown" in {
    forAll { (t1: Instant, c1: Coordinates, c2: Coordinates, n: Int) =>
      val offset = (n % 100).abs
      val (t2, t3) = (t1.plusSeconds(offset.toLong), t1.plusSeconds(100))
      val e = Ephemeris(t1 -> c1, t3 -> c2)
      e.get(t2) shouldEqual Some(c1.interpolate(c2, offset.toDouble / 100.00))
    }
  }

}
