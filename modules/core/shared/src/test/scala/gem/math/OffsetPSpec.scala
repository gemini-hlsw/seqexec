// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import gem.laws.discipline._
import gem.arb._
import monocle.law.discipline._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class OffsetPSpec extends CatsSuite {
  import ArbAngle._
  import ArbOffset._

  // Laws
  checkAll("Offset.P", CommutativeGroupTests[Offset.P].commutativeGroup)
  checkAll("Offset.P", OrderTests[Offset.P].order)
  checkAll("Offset.P.angle", IsoTests(Offset.P.angle))
  checkAll("Offset.P.signedArcseconds", SplitMonoTests(Offset.P.signedArcseconds).splitMono)

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
