// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Eq, Order, Show }
import cats.implicits._
import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
class RightAscensionSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbRightAscension._

  "Equality" must "be natural" in {
    forAll { (a: RightAscension, b: RightAscension) =>
      a.equals(b) shouldEqual Eq[RightAscension].eqv(a, b)
    }
  }

  "Order" must "be consistent with .toHourAngle.toMicroarcseconds" in {
    forAll { (a: RightAscension, b: RightAscension) =>
      Order[Long].comparison(a.toHourAngle.toMicroarcseconds, b.toHourAngle.toMicroarcseconds) shouldEqual
      Order[RightAscension].comparison(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: RightAscension) =>
      a.toString shouldEqual Show[RightAscension].show(a)
    }
  }

  "Conversion to HourAngle" must "be invertable" in {
    forAll { (a: RightAscension) =>
      RightAscension.fromHourAngle(a.toHourAngle) shouldEqual a
    }
  }

}
