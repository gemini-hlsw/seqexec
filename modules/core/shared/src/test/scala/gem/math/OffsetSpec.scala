// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import gem.arb._
import monocle.law.discipline._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class OffsetSpec extends CatsSuite {
  import ArbAngle._
  import ArbOffset._

  // Laws
  checkAll("Offset", CommutativeGroupTests[Offset].commutativeGroup)
  checkAll("Offset", OrderTests[Offset].order)
  checkAll("Offset.p", LensTests(Offset.p))
  checkAll("Offset.q", LensTests(Offset.q))
  checkAll("Offset.pAngle", LensTests(Offset.pAngle))
  checkAll("Offset.qAngle", LensTests(Offset.qAngle))

  test("Equality must be natural") {
    forAll { (a: Offset, b: Offset) =>
      a.equals(b) shouldEqual Eq[Offset].eqv(a, b)
    }
  }

  test("it must operate pairwise") {
    forAll { (a: Offset, b: Offset) =>
      Eq[Offset.P].eqv(a.p, b.p) &&
      Eq[Offset.Q].eqv(a.q, b.q) shouldEqual Eq[Offset].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Offset) =>
      a.toString shouldEqual Show[Offset].show(a)
    }
  }

  test("Conversion to components must be invertable") {
    forAll { (o: Offset) =>
      val (p, q) = (o.p, o.q)
      Offset(p, q) shouldEqual o
    }
  }

}
