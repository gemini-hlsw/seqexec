// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.common

import cats.tests.CatsSuite
import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.{ FunctorTests, TraverseTests }
import monocle.law.discipline.TraversalTests
import seqexec.common.ArbitrariesCommon._

/**
  * Tests the Zipper typeclasses
  */
final class ZipperSpec extends CatsSuite {

  test("support modify") {
    forAll { (l: List[Int], r: List[Int]) =>
      val z = Zipper(l, 0, r)
      assert(z.modify(_ => 1) !== z)
    }
  }
  test("Zipper length") {
    forAll { (l: Zipper[Int]) =>
      assert(l.length > 0)
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
      val m = l.forall(x => e.exists(_.focus === x)) ||
        r.headOption.forall(x => e.exists(_.focus === x))
      assert(m)
    }
  }

  checkAll("Functor[Zipper]", FunctorTests[Zipper].functor[Int, Int, Int])
  checkAll("Traversable[Zipper]",
           TraverseTests[Zipper].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Eq[Zipper]", EqTests[Zipper[Int]].eqv)
  checkAll("Zipper.zipperT", TraversalTests(Zipper.zipperT[Int]))
  // The zippers are unlawful
  // checkAll("Zipper.filterValue", TraversalTests(Zipper.filterValue[Int](_ % 2 === 0)))
  // checkAll("Zipper.filterZ", TraversalTests(Zipper.filterZ[Int](_ % 2 === 0)))
}
