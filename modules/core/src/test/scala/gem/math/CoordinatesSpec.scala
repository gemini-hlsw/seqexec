// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Eq, Show }
import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
class CoordinatesSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbCoordinates._
  import ArbRightAscension._
  import ArbDeclination._
  import ArbAngle._

  "Equality" must "be natural" in {
    forAll { (a: Coordinates, b: Coordinates) =>
      a.equals(b) shouldEqual Eq[Coordinates].eqv(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: Coordinates) =>
      a.toString shouldEqual Show[Coordinates].show(a)
    }
  }

  "offsetWithCarry" must "be consistent with offset" in {
    forAll { (a: Coordinates, dRA: HourAngle, dDec: Angle) =>
      a.offset(dRA, dDec) shouldEqual a.offsetWithCarry(dRA, dDec)._1
    }
  }

  it must "be invertable" in {
    forAll { (a: Coordinates, dRA: HourAngle, dDec: Angle) =>
      a.offsetWithCarry(dRA, dDec) match {
        case (cs, false) => cs.offset(-dRA, -dDec) shouldEqual a
        case (cs, true)  => cs.offset(-dRA,  dDec) shouldEqual a
      }
    }
  }

  "diff" must "be consistent with offset" in {
    forAll { (a: Coordinates, b: Coordinates) =>
      val (dRA, dDec) = a diff b
      a.offset(dRA, dDec) shouldEqual b
    }
  }

  it must "be consistent with offsetWithCarry, and never carry" in {
    forAll { (a: Coordinates, b: Coordinates) =>
      val (dRA, dDec) = a diff b
      a.offsetWithCarry(dRA, dDec).shouldEqual((b, false))
    }
  }

  "angularDistance" must "be in [0, 180°]" in {
    forAll { (a: Coordinates, b: Coordinates) =>
      a.angularDistance(b).toMicroarcseconds should be <= Angle.Angle180.toMicroarcseconds
    }
  }

  it must "be zero between any point and itself" in {
    forAll { (c: Coordinates) =>
      c.angularDistance(c) shouldEqual Angle.Angle0
    }
  }

  it must "be symmetric to within 1µas" in {
    forAll { (a: Coordinates, b: Coordinates) =>
      val Δ = a.angularDistance(b) - b.angularDistance(a)
      Δ.toSignedMicroarcseconds.abs should be <= 1L
    }
  }

  it must "be exactly 180° between the poles, regardless of RA" in {
    forAll { (ra1: RA, ra2: RA) =>
      val s = Coordinates(ra1, Dec.Min)
      val n = Coordinates(ra2, Dec.Max)
      n.angularDistance(s) shouldEqual Angle.Angle180
    }
  }

  it must "be exactly 90° between either pole and any point on the equator, regardless of RA" in {
    forAll { (ra1: RA, ra2: RA, dec: Dec, b: Boolean) =>
      val pole  = Coordinates(ra1, if (b) Dec.Min else Dec.Max)
      val point = Coordinates(ra2, Dec.Zero)
      val delta = point.angularDistance(pole)
      delta shouldEqual Angle.Angle90
    }
  }

  it must "equal any offset in declination from either pole, regardless of RA, to within 1µas" in {
    forAll { (ra1: RA, ra2: RA, dec: Dec, b: Boolean) =>
      val pole = Coordinates(ra1, if (b) Dec.Min else Dec.Max)
      val Δdec = dec.toAngle + Angle.Angle90 // [0, 180]
      val Δ    = pole.angularDistance(pole.offset(ra2.toHourAngle, Δdec)) - Δdec
      Δ.toSignedMicroarcseconds.abs should be <= 1L
    }
  }


  it must "equal any offset in right ascension along the equator, to within 1µas" in {
    forAll { (ra: RA, ha: HourAngle) =>
      val a = Coordinates(ra, Dec.Zero)
      val b = a.offset(ha, Angle.Angle0)
      val d = a.angularDistance(b)
      val Δ = d.toSignedMicroarcseconds.abs - ha.toSignedMicroarcseconds.abs
      Δ.abs should be <= 1L
    }
  }

  "interpolate" should "result in angular distance of 0° from `a` for factor 0.0, within 1µas" in {
    forAll { (a: Coordinates, b: Coordinates) =>
      val Δ = a.angularDistance(a.interpolate(b, 0.0))
      Δ.toSignedMicroarcseconds.abs should be <= 1L
    }
  }

  it should "result in angular distance of 0° from `b` for factor 1.0, within 1µas" in {
    forAll { (a: Coordinates, b: Coordinates) =>
      val Δ = b.angularDistance(a.interpolate(b, 1.0))
      Δ.toSignedMicroarcseconds.abs should be <= 1L
    }
  }

  it should "be consistent with fractional angular separation, to within 1µsec (15 µas)" in
    forAll { (c1: Coordinates, c2: Coordinates) =>
      val sep = c1.angularDistance(c2)
      val Δs  = (-1.0 to 2.0 by 0.1).map { f =>
        val stepSep = c1.interpolate(c2, f).angularDistance(c1)
        (stepSep.toMicroarcseconds - (sep.toSignedMicroarcseconds * f.abs).toLong)
      }
      Δs.filter(_ > 15L) shouldBe empty
    }

}
