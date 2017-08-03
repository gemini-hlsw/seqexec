// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

import scalaz.{ Equal, Show }
import scalaz.std.anyVal._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
class DeclinationSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbDeclination._
  import ArbAngle._

  "Equality" must "be natural" in {
    forAll { (a: Declination, b: Declination) =>
      a.equals(b) shouldEqual Equal[Declination].equal(a, b)
    }
  }

  "Equal" must "be consistent with .toAngle.toMicroarcseconds" in {
    forAll { (a: Declination, b: Declination) =>
      Equal[Long].equal(a.toAngle.toMicroarcseconds, b.toAngle.toMicroarcseconds) shouldEqual
      Equal[Declination].equal(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: Declination) =>
      a.toString shouldEqual Show[Declination].shows(a)
    }
  }

  "Conversion to Angle" must "be invertable" in {
    forAll { (a: Declination) =>
      Declination.unsafeFromAngle(a.toAngle) shouldEqual a
    }
  }

  "Construction" must "be consistent between fromAngle and fromAngleWithCarry" in {
    forAll { (a: Angle) =>
      (Declination.fromAngle(a), Declination.fromAngleWithCarry(a)) match {
        case (Some(d), (dʹ, false)) => d shouldEqual dʹ
        case (None,    (d,  true))  => d.toAngle shouldEqual a.mirrorBy(Angle.Angle90)
        case _                      => fail("Unpossible")
      }
    }
  }

  "Offsetting" must "have an identity" in {
    forAll { (a: Declination) =>
      a.offset(Angle.Angle0).shouldEqual((a, false))
    }
  }

  it must "be invertible" in {
    forAll { (a: Declination, b: Angle) =>
      a.offset(b) match {
        case (aʹ, false) => aʹ.offset(-b).shouldEqual((a, false))
        case (aʹ, true)  => aʹ.offset(b).shouldEqual((a, true))
      }
    }
  }

}
