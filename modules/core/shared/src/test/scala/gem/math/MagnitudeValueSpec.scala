// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import cats.{ Eq, Show, Order }
import cats.kernel.laws.discipline._
import gem.arb._

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
final class MagnitudeValueSpec extends CatsSuite {
  import ArbMagnitudeValue._

  // Laws
  checkAll("MagnitudeValue", OrderTests[MagnitudeValue].eqv)

  test("Equality must be natural") {
    forAll { (a: MagnitudeValue, b: MagnitudeValue) =>
      a.equals(b) shouldEqual Eq[MagnitudeValue].eqv(a, b)
    }
  }

  test("Order must be consistent with .value") {
    forAll { (a: MagnitudeValue, b: MagnitudeValue) =>
      Order[Int].comparison(a.value, b.value) shouldEqual
      Order[MagnitudeValue].comparison(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: MagnitudeValue) =>
      a.toString shouldEqual Show[MagnitudeValue].show(a)
    }
  }

}
