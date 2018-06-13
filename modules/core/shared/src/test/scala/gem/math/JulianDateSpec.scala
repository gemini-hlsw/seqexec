// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite

import gem.arb._

import java.time.LocalDateTime


@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class JulianDateSpec extends CatsSuite {

  import ArbJulianDate._
  import ArbTime._

  // Laws
  checkAll("JulianDate", OrderTests[JulianDate].order)

  test("JulianDate.eq.natural") {
    forAll { (a: JulianDate, b: JulianDate) =>
      a.equals(b) shouldEqual Eq[JulianDate].eqv(a, b)
    }
  }

  test("JulianDate.show.natural") {
    forAll { (a: JulianDate) =>
      a.toString shouldEqual Show[JulianDate].show(a)
    }
  }

  // Old Epoch.Scheme.toJulianDay algorithm
  private def oldEpochSchemeJulianDate(dt: LocalDateTime): Double = {
    import scala.math.floor

    val a = floor((14.0 - dt.getMonthValue) / 12.0)
    val y = dt.getYear + 4800.0 - a
    val m = dt.getMonthValue + 12 * a - 3.0
    dt.getDayOfMonth +
    floor((153.0 * m + 2.0) / 5.0) +
    365 * y +
    floor(y / 4.0) -
    floor(y / 100.0) +
    floor(y / 400.0) -
    32045.0
  }

  test("JulianDate.dayNumber matches old calculation") {
    forAll { (ldt: LocalDateTime) =>
      val jd0 = oldEpochSchemeJulianDate(ldt)
      val jd1 = JulianDate.ofLocalDateTime(ldt).dayNumber.toDouble

      jd0 shouldEqual jd1
    }
  }

  test("Some specific dates compared to USNO calculations") {

    // See http://aa.usno.navy.mil/data/docs/JulianDate.php
    val tests = List(
      (LocalDateTime.of(1918, 11, 11, 11,  0,  0), 2421908.958333),
      (LocalDateTime.of(1969,  7, 21,  2, 56, 15), 2440423.622396),
      (LocalDateTime.of(2001,  9, 11,  8, 46,  0), 2452163.865278),
      (LocalDateTime.of(2345,  6,  7, 12,  0,  0), 2577711.000000)
    )

    tests.foreach { case (ldt, expected) =>
      val jd  = JulianDate.ofLocalDateTime(ldt)
      assert((expected - jd.toDouble).abs <= 0.000001)
    }
  }

  test("Modified JulianDate should almost equal JulianDate - 2400000.5") {
    forAll { (j: JulianDate) =>
      assert(j.toModifiedDouble === (j.toDouble - 2400000.5) +- 0.000000001)
    }
  }
}
