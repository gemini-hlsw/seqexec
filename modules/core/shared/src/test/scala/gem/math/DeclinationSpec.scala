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
final class DeclinationSpec extends CatsSuite {
  import ArbDeclination._
  import ArbAngle._

  // Laws
  checkAll("Declination", OrderTests[Declination].order)
  checkAll("fromAngle", PrismTests(Declination.fromAngle))
  checkAll("fromStringHMS", FormatTests(Declination.fromStringSignedDMS).formatWith(ArbAngle.stringsDMS))

  test("Equality must be natural") {
    forAll { (a: Declination, b: Declination) =>
      a.equals(b) shouldEqual Eq[Declination].eqv(a, b)
    }
  }

  test("Eq must be consistent with .toAngle.toMicroarcseconds") {
    forAll { (a: Declination, b: Declination) =>
      Eq[Long].eqv(a.toAngle.toMicroarcseconds, b.toAngle.toMicroarcseconds) shouldEqual
      Eq[Declination].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Declination) =>
      a.toString shouldEqual Show[Declination].show(a)
    }
  }

  test("Construction must be consistent between fromAngle and fromAngleWithCarry") {
    forAll { (a: Angle) =>
      (Declination.fromAngle.getOption(a), Declination.fromAngleWithCarry(a)) match {
        case (Some(d), (dʹ, false)) => d shouldEqual dʹ
        case (None,    (d,  true))  => d.toAngle shouldEqual a.mirrorBy(Angle.Angle90)
        case _                      => fail("Unpossible")
      }
    }
  }

  test("Offsetting must have an identity") {
    forAll { (a: Declination) =>
      a.offset(Angle.Angle0).shouldEqual((a, false))
    }
  }

  test("Offsetting must be invertible") {
    forAll { (a: Declination, b: Angle) =>
      a.offset(b) match {
        case (aʹ, false) => aʹ.offset(-b).shouldEqual((a, false))
        case (aʹ, true)  => aʹ.offset(b).shouldEqual((a, true))
      }
    }
  }

}
