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
final class OffsetQSpec extends CatsSuite {
  import ArbAngle._
  import ArbOffset._

  // Laws
  checkAll("Offset.Q", CommutativeGroupTests[Offset.Q].commutativeGroup)
  checkAll("Offset.Q", OrderTests[Offset.Q].order)
  checkAll("Offset.Q.angle", IsoTests(Offset.Q.angle))
  checkAll("Offset.Q.signedArcseconds", SplitMonoTests(Offset.Q.signedArcseconds).splitMono)

  test("Equality must be natural") {
    forAll { (a: Offset.Q, b: Offset.Q) =>
      a.equals(b) shouldEqual Eq[Offset.Q].eqv(a, b)
    }
  }

  test("Equality be consistent with .toAngle") {
    forAll { (a: Offset.Q, b: Offset.Q) =>
      Eq[Angle].eqv(a.toAngle, b.toAngle) shouldEqual Eq[Offset.Q].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Offset.Q) =>
      a.toString shouldEqual Show[Offset.Q].show(a)
    }
  }

  test("Conversion to angle must be invertable") {
    forAll { (p: Offset.Q) =>
      Offset.Q(p.toAngle) shouldEqual p
    }
  }

}
