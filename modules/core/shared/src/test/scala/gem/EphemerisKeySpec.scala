// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.arb._

import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import gsp.math.laws.discipline._

final class EphemerisKeySpec extends CatsSuite {
  import ArbEphemerisKey._
  import ArbEnumerated._

  // Laws
  checkAll("EphemerisKey", OrderTests[EphemerisKey].order)
  checkAll("fromString", FormatTests(EphemerisKey.fromString).formatWith(ArbEphemerisKey.strings))
  checkAll("fromTypeAndDes", FormatTests(EphemerisKey.fromTypeAndDes).formatWith(ArbEphemerisKey.keyAndDes))

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

}
