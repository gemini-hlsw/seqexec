// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Eq, Show, Monoid }
import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
class OffsetQSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbOffset._

  // Compilation test
  protected val a0 = implicitly[Monoid[Offset.Q]]
  protected val a1 = implicitly[Show[Offset.Q]]
  protected val a2 = implicitly[Eq[Offset.Q]]

  "Equality" must "be natural" in {
    forAll { (a: Offset.Q, b: Offset.Q) =>
      a.equals(b) shouldEqual Eq[Offset.Q].eqv(a, b)
    }
  }

  it must "be consistent with .toAngle" in {
    forAll { (a: Offset.Q, b: Offset.Q) =>
      Eq[Angle].eqv(a.toAngle, b.toAngle) shouldEqual Eq[Offset.Q].eqv(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: Offset.Q) =>
      a.toString shouldEqual Show[Offset.Q].show(a)
    }
  }

  "Conversion to angle" must "be invertable" in {
    forAll { (q: Offset.Q) =>
      Offset.Q(q.toAngle) shouldEqual q
    }
  }

  "Offset.Q forms an Abelian Group over addition. It" must "be associative" in {
    forAll { (q: Offset.Q, b: Offset.Q, c: Offset.Q) =>
      (q + b) + c shouldEqual q + (b + c)
    }
  }

  it must "be commutative" in {
    forAll { (q: Offset.Q, b: Offset.Q) =>
      q + b shouldEqual b + q
    }
  }

  it must "have a left identity" in {
    forAll { (q: Offset.Q) =>
      q + Offset.Q.Zero shouldEqual q
    }
  }

  it must "have a right identity" in {
    forAll { (q: Offset.Q) =>
      Offset.Q.Zero + q shouldEqual q
    }
  }

  it must "have an inverse" in {
    forAll { (q: Offset.Q) =>
      q + (-q) shouldEqual Offset.Q.Zero
    }
  }

}
