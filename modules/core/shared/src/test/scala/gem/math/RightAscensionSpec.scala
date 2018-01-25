// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import cats.{ Eq, Show, Order }
import cats.kernel.laws.discipline._
import gem.arb._
import gem.laws.discipline._
import monocle.law.discipline._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class RightAscensionSpec extends CatsSuite {
  import ArbRightAscension._
  import ArbAngle._

  // Laws
  checkAll("RightAscension", OrderTests[RightAscension].order)
  checkAll("fromAngle", PrismTests(RightAscension.fromAngle))
  checkAll("fromHourAngle", IsoTests(RightAscension.fromHourAngle))
  checkAll("fromStringHMS", FormatTests(RightAscension.fromStringHMS).formatWith(ArbAngle.stringsHMS))

  test("Equality must be natural") {
    forAll { (a: RightAscension, b: RightAscension) =>
      a.equals(b) shouldEqual Eq[RightAscension].eqv(a, b)
    }
  }

  test("Order must be consistent with .toHourAngle.toMicroarcseconds") {
    forAll { (a: RightAscension, b: RightAscension) =>
      Order[Long].comparison(a.toHourAngle.toMicroarcseconds, b.toHourAngle.toMicroarcseconds) shouldEqual
      Order[RightAscension].comparison(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: RightAscension) =>
      a.toString shouldEqual Show[RightAscension].show(a)
    }
  }

}
