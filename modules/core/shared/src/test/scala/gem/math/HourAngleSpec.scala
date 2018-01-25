// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import gem.arb._
import gem.laws.discipline._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class HourAngleSpec extends CatsSuite {
  import ArbAngle._

  // Laws
  checkAll("HourAngle", CommutativeGroupTests[HourAngle].commutativeGroup)
  checkAll("HourAngle", EqTests[HourAngle].eqv)
  checkAll("fromStringHMS", FormatTests(HourAngle.fromStringHMS).formatWith(ArbAngle.stringsHMS))

  test("Equality must be natural") {
    forAll { (a: HourAngle, b: HourAngle) =>
      a.equals(b) shouldEqual Eq[HourAngle].eqv(a, b)
    }
  }

  test("Equality must be consistent with .toMicroseconds") {
    forAll { (a: HourAngle, b: HourAngle) =>
      Eq[Long].eqv(a.toMicroseconds, b.toMicroseconds) shouldEqual
      Eq[HourAngle].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: HourAngle) =>
      a.toString shouldEqual Show[HourAngle].show(a)
    }
  }

  test("Conversion to HMS must be invertable") {
    forAll { (a: HourAngle) =>
      val hms = a.toHMS
      HourAngle.fromHMS(
        hms.hours,
        hms.minutes,
        hms.seconds,
        hms.milliseconds,
        hms.microseconds
      ) shouldEqual a
    }
  }

  test("Widening to Angle must be invertable") {
    forAll { (a: HourAngle) =>
      a.toAngle.toHourAngle shouldEqual a
    }
  }

  test("Widening to Angle must also work for toHourAngleExact") {
    forAll { (a: HourAngle) =>
      a.toAngle.toHourAngleExact shouldEqual Some(a)
    }
  }

  test("Flipping must be invertable") {
    forAll { (a: HourAngle) =>
      a.flip.flip shouldEqual a
    }
  }

  test("Construction must normalize [non-pathological] angles") {
    forAll { (a: HourAngle, n: Int) =>
      val factor   = n % 10
      val msIn24 = 24L * 60L * 60L * 1000L * 1000L
      val offset   = msIn24 * factor
      val b = HourAngle.fromMicroseconds(a.toMicroseconds + offset)
      a shouldEqual b
    }
  }

}
