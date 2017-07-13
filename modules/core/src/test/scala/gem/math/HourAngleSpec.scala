// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz.{ Equal, Monoid, Show }
import scalaz.std.anyVal._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
class HourAngleSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbAngle._

  // Compilation test
  protected val a0 = implicitly[Monoid[HourAngle]]
  protected val a1 = implicitly[Equal[HourAngle]]
  protected val a2 = implicitly[Show[HourAngle]]

  "Equality" must "be natural" in {
    forAll { (a: HourAngle, b: HourAngle) =>
      a.equals(b) shouldEqual Equal[HourAngle].equal(a, b)
    }
  }

  it must "be consistent with .toMicroseconds" in {
    forAll { (a: HourAngle, b: HourAngle) =>
      Equal[Long].equal(a.toMicroseconds, b.toMicroseconds) shouldEqual
      Equal[HourAngle].equal(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: HourAngle) =>
      a.toString shouldEqual Show[HourAngle].shows(a)
    }
  }

  "Conversion to HMS" must "be invertable" in {
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

  "Widening to Angle" must "be invertable" in {
    forAll { (a: HourAngle) =>
      a.toAngle.toHourAngle shouldEqual a
    }
  }

  it must "also work for toHourAngleExact" in {
    forAll { (a: HourAngle) =>
      a.toAngle.toHourAngleExact shouldEqual Some(a)
    }
  }

  "Flipping" must "be invertable" in {
    forAll { (a: HourAngle) =>
      a.flip.flip shouldEqual a
    }
  }

  "HourAngle forms an Abelian Group over addition. It" must "be associative" in {
    forAll { (a: HourAngle, b: HourAngle, c: HourAngle) =>
      (a + b) + c shouldEqual a + (b + c)
    }
  }

  it must "be commutative" in {
    forAll { (a: HourAngle, b: HourAngle) =>
      a + b shouldEqual b + a
    }
  }

  it must "have a left identity" in {
    forAll { (a: HourAngle) =>
      a + HourAngle.HourAngle0 shouldEqual a
    }
  }

  it must "have a right identity" in {
    forAll { (a: HourAngle) =>
      HourAngle.HourAngle0 + a shouldEqual a
    }
  }

  it must "have an inverse" in {
    forAll { (a: HourAngle) =>
      a + (-a) shouldEqual HourAngle.HourAngle0
    }
  }

  "Construction" must "normalize [non-pathological] angles" in {
    forAll { (a: HourAngle, n: Int) =>
      val factor   = n % 10
      val msIn24 = 24L * 60L * 60L * 1000L * 1000L
      val offset   = msIn24 * factor
      val b = HourAngle.fromMicroseconds(a.toMicroseconds + offset)
      a shouldEqual b
    }
  }

}
