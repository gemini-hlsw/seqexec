// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import gem.laws.discipline._
import gem.arb._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class CoordinatesSpec extends CatsSuite {
  import ArbCoordinates._
  import ArbRightAscension._
  import ArbDeclination._
  import ArbAngle._

  // Laws
  checkAll("Coordinates", OrderTests[Coordinates].order)
  checkAll("Formats.HmsDms", FormatTests(Coordinates.Optics.fromHmsDms).formatWith(ArbCoordinates.strings))

  test("Equality must be natural") {
    forAll { (a: Coordinates, b: Coordinates) =>
      a.equals(b) shouldEqual Eq[Coordinates].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Coordinates) =>
      a.toString shouldEqual Show[Coordinates].show(a)
    }
  }

  test("offsetWithCarry must be consistent with offset") {
    forAll { (a: Coordinates, dRA: HourAngle, dDec: Angle) =>
      a.offset(dRA, dDec) shouldEqual a.offsetWithCarry(dRA, dDec)._1
    }
  }

  test("offsetWithCarry must be invertable") {
    forAll { (a: Coordinates, dRA: HourAngle, dDec: Angle) =>
      a.offsetWithCarry(dRA, dDec) match {
        case (cs, false) => cs.offset(-dRA, -dDec) shouldEqual a
        case (cs, true)  => cs.offset(-dRA,  dDec) shouldEqual a
      }
    }
  }

  test("diff must be consistent with offset") {
    forAll { (a: Coordinates, b: Coordinates) =>
      val (dRA, dDec) = a diff b
      a.offset(dRA, dDec) shouldEqual b
    }
  }

  test("diff must be consistent with offsetWithCarry, and never carry") {
    forAll { (a: Coordinates, b: Coordinates) =>
      val (dRA, dDec) = a diff b
      a.offsetWithCarry(dRA, dDec).shouldEqual((b, false))
    }
  }

  test("angularDistance must be in [0, 180°]") {
    forAll { (a: Coordinates, b: Coordinates) =>
      a.angularDistance(b).toMicroarcseconds should be <= Angle.Angle180.toMicroarcseconds
    }
  }

  test("angularDistance must be zero between any point and itself") {
    forAll { (c: Coordinates) =>
      c.angularDistance(c) shouldEqual Angle.Angle0
    }
  }

  test("angularDistance must be symmetric to within 1µas") {
    forAll { (a: Coordinates, b: Coordinates) =>
      val Δ = a.angularDistance(b) - b.angularDistance(a)
      Δ.toSignedMicroarcseconds.abs should be <= 1L
    }
  }

  test("angularDistance must be exactly 180° between the poles, regardless of RA") {
    forAll { (ra1: RA, ra2: RA) =>
      val s = Coordinates(ra1, Dec.Min)
      val n = Coordinates(ra2, Dec.Max)
      n.angularDistance(s) shouldEqual Angle.Angle180
    }
  }

  test("angularDistance must be exactly 90° between either pole and any point on the equator, regardless of RA") {
    forAll { (ra1: RA, ra2: RA, dec: Dec, b: Boolean) =>
      val pole  = Coordinates(ra1, if (b) Dec.Min else Dec.Max)
      val point = Coordinates(ra2, Dec.Zero)
      val delta = point.angularDistance(pole)
      delta shouldEqual Angle.Angle90
    }
  }

  test("angularDistance must equal any offset in declination from either pole, regardless of RA, to within 1µas") {
    forAll { (ra1: RA, ra2: RA, dec: Dec, b: Boolean) =>
      val pole = Coordinates(ra1, if (b) Dec.Min else Dec.Max)
      val Δdec = dec.toAngle + Angle.Angle90 // [0, 180]
      val Δ    = pole.angularDistance(pole.offset(ra2.toHourAngle, Δdec)) - Δdec
      Δ.toSignedMicroarcseconds.abs should be <= 1L
    }
  }


  test("angularDistance must equal any offset in right ascension along the equator, to within 1µas") {
    forAll { (ra: RA, ha: HourAngle) =>
      val a = Coordinates(ra, Dec.Zero)
      val b = a.offset(ha, Angle.Angle0)
      val d = a.angularDistance(b)
      val Δ = d.toSignedMicroarcseconds.abs - ha.toSignedMicroarcseconds.abs
      Δ.abs should be <= 1L
    }
  }

  test("interpolate should result in angular distance of 0° from `a` for factor 0.0, within 1µsec (15µas)") {
    forAll { (a: Coordinates, b: Coordinates) =>
      val Δ = a.angularDistance(a.interpolate(b, 0.0))
      Δ.toSignedMicroarcseconds.abs should be <= 15L
    }
  }

  test("interpolate should result in angular distance of 0° from `b` for factor 1.0, within 1µsec (15µas)") {
    forAll { (a: Coordinates, b: Coordinates) =>
      val Δ = b.angularDistance(a.interpolate(b, 1.0))
      Δ.toSignedMicroarcseconds.abs should be <= 15L
    }
  }

  test("interpolate should be consistent with fractional angular separation, to within 20 µas") {
    val µas180 = Angle.Angle180.toMicroarcseconds
    val µas360 = µas180 * 2L

    forAll { (c1: Coordinates, c2: Coordinates) =>
      val sep = c1.angularDistance(c2)
      val Δs  = (-1.0 to 2.0 by 0.1).map { f =>
        val stepSep  = c1.interpolate(c2, f).angularDistance(c1).toMicroarcseconds
        val fracSep  = (sep.toMicroarcseconds * f.abs).toLong
        val fracSepʹ = if (fracSep <= µas180) fracSep else µas360 - fracSep
        (stepSep - fracSepʹ).abs
      }
      Δs.filter(_ > 20L) shouldBe empty
    }

  }

}
