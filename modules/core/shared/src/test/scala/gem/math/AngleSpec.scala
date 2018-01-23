// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import gem.arb._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class AngleSpec extends CatsSuite {
  import ArbAngle._

  // Laws
  checkAll("Angle", CommutativeGroupTests[Angle].commutativeGroup)
  checkAll("Angle", EqTests[Angle].eqv)
  checkAll("Angle", OrderTests[Angle](Angle.AngleOrder).order)
  checkAll("SignedAngle", OrderTests[Angle](Angle.SignedAngleOrder).order)

  test("Equality must be natural") {
    forAll { (a: Angle, b: Angle) =>
      a.equals(b) shouldEqual Eq[Angle].eqv(a, b)
    }
  }

  test("Equality must be consistent with .toMicroarcseconds") {
    forAll { (a: Angle, b: Angle) =>
      Eq[Long].eqv(a.toMicroarcseconds, b.toMicroarcseconds) shouldEqual
      Eq[Angle].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Angle) =>
      a.toString shouldEqual Show[Angle].show(a)
    }
  }

  test("Conversion to DMS must be invertable") {
    forAll { (a: Angle) =>
      val dms = a.toDMS
      Angle.fromDMS(
        dms.degrees,
        dms.arcminutes,
        dms.arcseconds,
        dms.milliarcseconds,
        dms.microarcseconds
      ) shouldEqual a
    }
  }

  test("Conversion to signed microarcseconds must be invertable") {
    forAll { (a: Angle) =>
      Angle.fromMicroarcseconds(a.toSignedMicroarcseconds) shouldEqual a
    }
  }

  test("Narrowing to HourAngle must be invertable where defined") {
    forAll { (a: Angle) =>
      a.toHourAngleExact match {
        case Some(b) => a shouldEqual b
        case None    => succeed
      }
    }
  }

  test("Flipping must be invertable") {
    forAll { (a: Angle) =>
      a.flip.flip shouldEqual a
    }
  }

  test("Construction must normalize [non-pathological] angles") {
    forAll { (a: Angle, n: Int) =>
      val factor   = n % 10
      val masIn360 = 360L * 60L * 60L * 1000L * 1000L
      val offset   = masIn360 * factor
      val b = Angle.fromMicroarcseconds(a.toMicroarcseconds + offset)
      a shouldEqual b
    }
  }

  // In principle I think this is the only thing we need to check.
  test("mirrorBy must obey the mirror law") {
    forAll { (a: Angle, b: Angle) =>
      b - a shouldEqual (a mirrorBy b) - b
    }
  }

  test("mirrorBy must be reflexive") {
    forAll { (a: Angle) =>
      a mirrorBy a shouldEqual a
    }
  }

  test("mirrorBy must be invertible") {
    forAll { (a: Angle, b: Angle) =>
      a.mirrorBy(b).mirrorBy(b) shouldEqual a
    }
  }

  test("mirrorBy must be invariant to flip in mirror angle") {
    forAll { (a: Angle, b: Angle) =>
      a.mirrorBy(b) shouldEqual a.mirrorBy(b.flip)
    }
  }

  test("mirrorBy must distribute over flip in target angle") {
    forAll { (a: Angle, b: Angle) =>
      (a mirrorBy b).flip shouldEqual (a.flip mirrorBy b)
    }
  }

  test("mirrorBy must be consistent with flip") {
    forAll { (a: Angle) =>
      a.mirrorBy(a + Angle.Angle90) shouldEqual a.flip
    }
  }

  test("mirrorBy must be consistent with unary negation") {
    forAll { (a: Angle) =>
      a.mirrorBy(Angle.Angle0) shouldEqual -a
    }
  }

  test("formatDMS and parseDMS must round-trip") {
    forAll { (a: Angle) =>
      Angle.parseDMS(a.formatDMS) shouldEqual Some(a)
    }
  }

  test("formatSignedDMS and parseSignedDMS must round-trip") {
    forAll { (a: Angle) =>
      Angle.parseSignedDMS(a.formatSignedDMS) shouldEqual Some(a)
    }
  }

}
