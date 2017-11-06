// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.arb.ArbEphemerisKey._

import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class EphemerisKeySpec extends CatsSuite {

  // Laws
  checkAll("EphemerisKey", EqTests[EphemerisKey].eqv)

  test("Equality must be natural") {
    forAll { (a: EphemerisKey, b: EphemerisKey) =>
      a.equals(b) shouldEqual Eq[EphemerisKey].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: EphemerisKey) =>
      a.toString shouldEqual Show[EphemerisKey].show(a)
    }
  }

  test("Round trip from String") {
    forAll { (a: EphemerisKey) =>
      EphemerisKey.unsafeFromString(a.format) shouldEqual a
    }
  }

  test("Round trip from type and des") {
    forAll { (a: EphemerisKey) =>
      EphemerisKey.unsafeFromTypeAndDes(a.keyType, a.des) shouldEqual a
    }
  }

}
