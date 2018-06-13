// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import gem.arb._
import gem.laws.discipline._
import monocle.law.discipline._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class HourAngleSpec extends CatsSuite {
  import ArbAngle._

  // Laws
  checkAll("HourAngle", CommutativeGroupTests[HourAngle].commutativeGroup)
  checkAll("HourAngle", EqTests[HourAngle].eqv)
  checkAll("HourAngle <: Angle", SubgroupTests[HourAngle, Angle].subgroup)

  // Optics
  checkAll("angle", SplitMonoTests(HourAngle.angle).splitMono)
  checkAll("microseconds", SplitMonoTests(HourAngle.microseconds).splitMono)
  checkAll("milliseconds", WedgeTests(HourAngle.milliseconds).wedge)
  checkAll("seconds", WedgeTests(HourAngle.seconds).wedge)
  checkAll("minutes", WedgeTests(HourAngle.minutes).wedge)
  checkAll("hours", WedgeTests(HourAngle.hours).wedge)
  checkAll("hms", IsoTests(HourAngle.hms))
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
      val hms = HourAngle.hms.get(a)
      HourAngle.fromHMS(
        hms.hours,
        hms.minutes,
        hms.seconds,
        hms.milliseconds,
        hms.microseconds
      ) shouldEqual a
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
