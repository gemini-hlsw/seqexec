// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

import scalaz.Monoid

class OffsetSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbOffset._

  // Compilation test
  protected val a0 = implicitly[Monoid[Offset]]

  "Conversion to components" must "be invertable" in {
    forAll { (o: Offset) =>
      val (p, q) = (o.p, o.q)
      Offset(p, q) shouldEqual o
    }
  }

  "Offset forms an Abelian Group over addition. It" must "be associative" in {
    forAll { (o: Offset, b: Offset, c: Offset) =>
      (o + b) + c shouldEqual o + (b + c)
    }
  }

  it must "be commutative" in {
    forAll { (o: Offset, b: Offset) =>
      o + b shouldEqual b + o
    }
  }

  it must "have a left identity" in {
    forAll { (o: Offset) =>
      o + Offset.Zero shouldEqual o
    }
  }

  it must "have a right identity" in {
    forAll { (o: Offset) =>
      Offset.Zero + o shouldEqual o
    }
  }

  it must "have an inverse" in {
    forAll { (o: Offset) =>
      o + (-o) shouldEqual Offset.Zero
    }
  }

}
