// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

import scalaz.Monoid

class OffsetPSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbOffset._

  // Compilation test
  protected val a0 = implicitly[Monoid[Offset.P]]

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
