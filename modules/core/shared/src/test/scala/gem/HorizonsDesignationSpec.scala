// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.arb.ArbHorizonsDesignation._

import cats.kernel.laws.OrderLaws
import cats.{Eq, Show}
import cats.tests.CatsSuite

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class HorizonsDesignationSpec extends CatsSuite {

  // Laws
  checkAll("HorizonsDesignation", OrderLaws[HorizonsDesignation].eqv)

  test("Equality must be natural") {
    forAll { (a: HorizonsDesignation, b: HorizonsDesignation) =>
      a.equals(b) shouldEqual Eq[HorizonsDesignation].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: HorizonsDesignation) =>
      a.toString shouldEqual Show[HorizonsDesignation].show(a)
    }
  }

  test("Round trip from String") {
    forAll { (a: HorizonsDesignation) =>
      HorizonsDesignation.unsafeFromString(a.format) shouldEqual a
    }
  }

  test("Round trip from type and des") {
    forAll { (a: HorizonsDesignation) =>
      HorizonsDesignation.unsafeFromTypeAndDes(a.horizonsType, a.des) shouldEqual a
    }
  }

}
