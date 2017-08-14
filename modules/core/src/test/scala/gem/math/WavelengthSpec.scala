// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Eq, Order, Show }
import cats.implicits._
import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
class WavelengthSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbWavelength._

  "Equality" must "be natural" in {
    forAll { (a: Wavelength, b: Wavelength) =>
      a.equals(b) shouldEqual Eq[Wavelength].eqv(a, b)
    }
  }

  "Order" must "be consistent with .toAngstroms" in {
    forAll { (a: Wavelength, b: Wavelength) =>
      Order[Int].comparison(a.toAngstroms, b.toAngstroms) shouldEqual
      Order[Wavelength].comparison(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: Wavelength) =>
      a.toString shouldEqual Show[Wavelength].show(a)
    }
  }

  "Conversion to angstroms" must "be invertable" in {
    forAll { (a: Wavelength) =>
      Wavelength.fromAngstroms(a.toAngstroms) shouldEqual Some(a)
    }
  }

  "Construction from an arbitrary Int" must "not allow negative values" in {
    forAll { (n: Int) =>
      Wavelength.fromAngstroms(n).isDefined shouldEqual n >= 0
    }
  }

}
