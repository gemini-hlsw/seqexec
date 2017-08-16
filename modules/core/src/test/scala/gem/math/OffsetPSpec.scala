// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import cats.kernel.laws._
import gem.arb._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class OffsetPSpec extends CatsSuite {
  import ArbOffset._

  // Laws
  checkAll("Offset.P", GroupLaws[Offset.P].commutativeGroup)
  checkAll("Offset.P", OrderLaws[Offset.P].eqv)

  test("Equality must be natural") {
    forAll { (a: Offset.P, b: Offset.P) =>
      a.equals(b) shouldEqual Eq[Offset.P].eqv(a, b)
    }
  }

  test("Equality be consistent with .toAngle") {
    forAll { (a: Offset.P, b: Offset.P) =>
      Eq[Angle].eqv(a.toAngle, b.toAngle) shouldEqual Eq[Offset.P].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Offset.P) =>
      a.toString shouldEqual Show[Offset.P].show(a)
    }
  }

  test("Conversion to angle must be invertable") {
    forAll { (p: Offset.P) =>
      Offset.P(p.toAngle) shouldEqual p
    }
  }

}
