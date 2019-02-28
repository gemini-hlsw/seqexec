// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import cats.{ Eq, Show, Order }
import cats.kernel.laws.discipline._
import gem.arb._
import gem.optics.Format
import monocle.law.discipline._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class WavelengthSpec extends CatsSuite {
  import ArbWavelength._

  // Laws
  checkAll("Wavelength",      OrderTests[Wavelength].order)
  checkAll("fromPicometers",  PrismTests(Wavelength.fromPicometers))

  // These are not valid `Format` because they don't round trip Wavelength -> Int -> Wavelength
  // Switching to bigger units loses precision.
//  checkAll("fromAngstroms",   FormatTests(Wavelength.fromAngstroms).formatWith(arbitrary[Int]))
//  checkAll("fromNanometers",  FormatTests(Wavelength.fromNanometers).formatWith(arbitrary[Int]))
//  checkAll("fromMicrometers", FormatTests(Wavelength.fromMicrometers).formatWith(arbitrary[Int]))

  test("Equality must be natural") {
    forAll { (a: Wavelength, b: Wavelength) =>
      a.equals(b) shouldEqual Eq[Wavelength].eqv(a, b)
    }
  }

  test("Order must be consistent with .toPicometers") {
    forAll { (a: Wavelength, b: Wavelength) =>
      Order[Int].comparison(a.toPicometers, b.toPicometers) shouldEqual
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
      Wavelength.fromPicometers.getOption(n).isDefined shouldEqual n >= 0
    }
  }

  private def pow10(exp: Int): Int =
    math.pow(10, exp.toDouble).toInt

  private def conversionTo(f: Format[Int, Wavelength], exp: Int): org.scalatest.Assertion =
    forAll { (a: Wavelength) =>
      f.reverseGet(a) shouldEqual a.toPicometers / pow10(exp)
    }

  private def conversionFrom(f: Format[Int, Wavelength], exp: Int): org.scalatest.Assertion =
    forAll(Gen.posNum[Int]) { (n: Int) =>
      f.unsafeGet(n).toPicometers shouldEqual n * pow10(exp)
    }

  test("Conversion λ => Int (Å)") {
    conversionTo(Wavelength.fromAngstroms, 2)
  }

  test("Conversion Int (Å) => λ") {
    conversionFrom(Wavelength.fromAngstroms, 2)
  }

  test("Conversion λ => Int (nm)") {
    conversionTo(Wavelength.fromNanometers, 3)
  }

  test("Conversion Int (nm) => λ") {
    conversionFrom(Wavelength.fromNanometers, 3)
  }

  test("Conversion λ => Int (μm)") {
    conversionTo(Wavelength.fromMicrometers, 6)
  }

  test("Conversion Int (μm) => λ") {
    conversionFrom(Wavelength.fromMicrometers, 6)
  }

}
