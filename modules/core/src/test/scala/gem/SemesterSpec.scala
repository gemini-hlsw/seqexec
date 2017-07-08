// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.arb._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class SemesterSpec extends FlatSpec with Matchers with PropertyChecks {
  import ArbSemester._

  ".format" should "be invertible via .fromString" in {
    forAll { (s: Semester) =>
      Semester.fromString(s.format) shouldEqual Some(s)
    }
  }

  ".plusYears" should "result in a properly updated year" in {
    forAll { (s: Semester, n: Short) =>
      val nʹ = n.toInt
      val sʹ = s.plusYears(nʹ)
      sʹ.year.getValue shouldEqual s.year.getValue + nʹ
    }
  }

  it should "never affect the half" in {
    forAll { (s: Semester, n: Short) =>
      val nʹ = n.toInt
      val sʹ = s.plusYears(nʹ)
      sʹ.half shouldEqual s.half
    }
  }

  it should "have an identity" in {
    forAll { (s: Semester) =>
      s.plusYears(0) shouldEqual s
    }
  }

  it should "be invertible by negation, for reasonable values" in {
    forAll { (s: Semester, n: Short) =>
      val nʹ = n.toInt
      s.plusYears(nʹ).plusYears(-nʹ) shouldEqual s
    }
  }

  it should "be a homomorphism over addition, for reasonable values" in {
    forAll { (s: Semester, n1: Short, n2: Short) =>
      val (n1ʹ, n2ʹ) = (n1.toInt, n2.toInt)
      s.plusYears(n1ʹ).plusYears(n2ʹ) shouldEqual s.plusYears(n1ʹ + n2ʹ)
    }
  }

  ".plusSemesters" should "be consistent with .plusYears, for reasonable values" in {
    forAll { (s: Semester, n: Byte, b: Boolean) =>
      val nʹ = n.toInt
      val bʹ = if (b) nʹ.signum else 0
      val s1 = s.plusYears(nʹ).plusSemesters(bʹ)
      val s2 = s.plusSemesters(nʹ * 2 + bʹ)
      s1 shouldEqual s2
    }
  }

  it should "have an identity" in {
    forAll { (s: Semester) =>
      s.plusSemesters(0) shouldEqual s
    }
  }

  it should "be invertible by negation, for reasonable values" in {
    forAll { (s: Semester, n: Byte) =>
      val nʹ = n.toInt
      s.plusSemesters(nʹ).plusSemesters(-nʹ) shouldEqual s
    }
  }

  it should "be a homomorphism over addition, for reasonable values" in {
    forAll { (s: Semester, n1: Byte, n2: Byte) =>
      val (n1ʹ, n2ʹ) = (n1.toInt, n2.toInt)
      s.plusSemesters(n1ʹ).plusSemesters(n2ʹ) shouldEqual s.plusSemesters(n1ʹ + n2ʹ)
    }
  }

}
