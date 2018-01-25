// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import cats.{ Eq, Show, Order }
import cats.kernel.laws.discipline._
import gem.arb._
import monocle.law.discipline._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class WavelengthSpec extends CatsSuite {
  import ArbWavelength._

  // Laws
  checkAll("Wavelength", OrderTests[Wavelength].order)
  checkAll("fromAngstroms", PrismTests(Wavelength.fromAngstroms))

  test("Equality must be natural") {
    forAll { (a: Wavelength, b: Wavelength) =>
      a.equals(b) shouldEqual Eq[Wavelength].eqv(a, b)
    }
  }

  test("Order must be consistent with .toAngstroms") {
    forAll { (a: Wavelength, b: Wavelength) =>
      Order[Int].comparison(a.toAngstroms, b.toAngstroms) shouldEqual
      Order[Wavelength].comparison(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Wavelength) =>
      a.toString shouldEqual Show[Wavelength].show(a)
    }
  }

  test("Construction from an arbitrary Int must not allow negative values") {
    forAll { (n: Int) =>
      Wavelength.fromAngstroms.getOption(n).isDefined shouldEqual n >= 0
    }
  }

}
