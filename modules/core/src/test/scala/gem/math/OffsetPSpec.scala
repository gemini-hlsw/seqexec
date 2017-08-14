// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Eq, Show, Monoid }
import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
class OffsetPSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbOffset._

  // Compilation test
  protected val a0 = implicitly[Monoid[Offset.P]]
  protected val a1 = implicitly[Show[Offset.P]]
  protected val a2 = implicitly[Eq[Offset.P]]

  "Equality" must "be natural" in {
    forAll { (a: Offset.P, b: Offset.P) =>
      a.equals(b) shouldEqual Eq[Offset.P].eqv(a, b)
    }
  }

  it must "be consistent with .toAngle" in {
    forAll { (a: Offset.P, b: Offset.P) =>
      Eq[Angle].eqv(a.toAngle, b.toAngle) shouldEqual Eq[Offset.P].eqv(a, b)
    }
  }

  "Show" must "be natural" in {
    forAll { (a: Offset.P) =>
      a.toString shouldEqual Show[Offset.P].show(a)
    }
  }

  "Conversion to angle" must "be invertable" in {
    forAll { (p: Offset.P) =>
      Offset.P(p.toAngle) shouldEqual p
    }
  }

  "Offset.P forms an Abelian Group over addition. It" must "be associative" in {
    forAll { (p: Offset.P, b: Offset.P, c: Offset.P) =>
      (p + b) + c shouldEqual p + (b + c)
    }
  }

  it must "be commutative" in {
    forAll { (p: Offset.P, b: Offset.P) =>
      p + b shouldEqual b + p
    }
  }

  it must "have a left identity" in {
    forAll { (p: Offset.P) =>
      p + Offset.P.Zero shouldEqual p
    }
  }

  it must "have a right identity" in {
    forAll { (p: Offset.P) =>
      Offset.P.Zero + p shouldEqual p
    }
  }

  it must "have an inverse" in {
    forAll { (p: Offset.P) =>
      p + (-p) shouldEqual Offset.P.Zero
    }
  }

}
