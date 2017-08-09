// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Eq, Monoid, Show }
import cats.implicits._
import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
class AngleSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbAngle._

  // Compilation test
  protected val a0 = implicitly[Monoid[Angle]]
  protected val a1 = implicitly[Eq[Angle]]
  protected val a2 = implicitly[Show[Angle]]

  "Equality" must "be natural" in {
    forAll { (a: Angle, b: Angle) =>
      a.equals(b) shouldEqual Eq[Angle].eqv(a, b)
    }
  }

  it must "be consistent with .toMicroarcseconds" in {
    forAll { (a: Angle, b: Angle) =>
      Eq[Long].eqv(a.toMicroarcseconds, b.toMicroarcseconds) shouldEqual
      Eq[Angle].eqv(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: Angle) =>
      a.toString shouldEqual Show[Angle].show(a)
    }
  }

  "Conversion to DMS" must "be invertable" in {
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

  "Conversion to signed microarcseconds" must "be invertable" in {
    forAll { (a: Angle) =>
      Angle.fromMicroarcseconds(a.toSignedMicroarcseconds) shouldEqual a
    }
  }

  "Narrowing to HourAngle" must "be invertable where defined" in {
    forAll { (a: Angle) =>
      a.toHourAngleExact match {
        case Some(b) => a shouldEqual b
        case None    => succeed
      }
    }
  }

  "Flipping" must "be invertable" in {
    forAll { (a: Angle) =>
      a.flip.flip shouldEqual a
    }
  }

  "Angle forms an Abelian Group over addition. It" must "be associative" in {
    forAll { (a: Angle, b: Angle, c: Angle) =>
      (a + b) + c shouldEqual a + (b + c)
    }
  }

  it must "be commutative" in {
    forAll { (a: Angle, b: Angle) =>
      a + b shouldEqual b + a
    }
  }

  it must "have a left identity" in {
    forAll { (a: Angle) =>
      a + Angle.Angle0 shouldEqual a
    }
  }

  it must "have a right identity" in {
    forAll { (a: Angle) =>
      Angle.Angle0 + a shouldEqual a
    }
  }

  it must "have an inverse" in {
    forAll { (a: Angle) =>
      a + (-a) shouldEqual Angle.Angle0
    }
  }

  "Construction" must "normalize [non-pathological] angles" in {
    forAll { (a: Angle, n: Int) =>
      val factor   = n % 10
      val masIn360 = 360L * 60L * 60L * 1000L * 1000L
      val offset   = masIn360 * factor
      val b = Angle.fromMicroarcseconds(a.toMicroarcseconds + offset)
      a shouldEqual b
    }
  }

  // In principle I think this is the only thing we need to check.
  "mirrorBy" must "obey the mirror law" in {
    forAll { (a: Angle, b: Angle) =>
      b - a shouldEqual (a mirrorBy b) - b
    }
  }

  it must "be reflexive" in {
    forAll { (a: Angle) =>
      a mirrorBy a shouldEqual a
    }
  }

  it must "be invertible" in {
    forAll { (a: Angle, b: Angle) =>
      a.mirrorBy(b).mirrorBy(b) shouldEqual a
    }
  }

  it must "be invariant to flip in mirror angle" in {
    forAll { (a: Angle, b: Angle) =>
      a.mirrorBy(b) shouldEqual a.mirrorBy(b.flip)
    }
  }

  it must "distribute over flip in target angle" in {
    forAll { (a: Angle, b: Angle) =>
      (a mirrorBy b).flip shouldEqual (a.flip mirrorBy b)
    }
  }

  it must "be consistent with flip" in {
    forAll { (a: Angle) =>
      a.mirrorBy(a + Angle.Angle90) shouldEqual a.flip
    }
  }

  it must "be consistent with unary negation" in {
    forAll { (a: Angle) =>
      a.mirrorBy(Angle.Angle0) shouldEqual -a
    }
  }

}
