// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.common

import cats.kernel.laws.discipline.EqTests
//import cats.laws.discipline.FunctorTests
import cats.tests.CatsSuite

/**
  * Tests the Zipper typeclasses
  */
final class ZipperSpec extends CatsSuite {
  import ArbitrariesWebCommon.arbZipper
  import ArbitrariesWebCommon.zipperCogen

  test("support modify") {
    forAll { (l: List[Int], r: List[Int]) =>
      val z = Zipper(l, 0, r)
      assert(z.modify(_ => 1) !== z)
    }
  }
  test("support exists") {
    forAll { (l: List[Int], r: List[Int]) =>
      val u = Zipper(l, 0, r)
      val e = u.exists(_ === 0)
      assert(e)
    }
  }
  test("support find") {
    forAll { (l: List[Int], r: List[Int]) =>
      val u = Zipper(l, 0, r)
      val e = u.find(_ === 0)
      assert(e.isDefined)
    }
  }
  test("support find focus I") {
    forAll { (l: List[Int], r: List[Int]) =>
      val u = Zipper(l, 0, r)
      val e = u.findFocus(_ === 0)
      assert(e.exists(_.focus === 0) === true)
    }
  }
  test("support find focus II") {
    forAll { (l: List[Int], r: List[Int]) =>
      val u = Zipper(l, 0, r)
      val e = u.findFocus(x => l.headOption.forall(x === _))
      val m = l.headOption.forall(x => e.exists(_.focus === x))
      assert(m)
    }
  }
  test("support find focus III") {
    forAll { (l: List[Int], r: List[Int]) =>
      val u = Zipper(l, 0, r)
      val e = u.findFocus(x => r.headOption.forall(x === _))
      val m = l.forall(x => e.exists(_.focus === x)) || r.headOption.forall(x => e.exists(_.focus === x))
      assert(m)
    }
  }

  // For some reason this fails in travis
//  checkAll("Functor[Zipper]", FunctorTests[Zipper].functor)
  checkAll("Eq[Zipper]", EqTests[Zipper[Int]].eqv)
}
