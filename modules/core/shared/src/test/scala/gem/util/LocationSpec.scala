// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package util

import gem.arb.ArbLocation

import cats.{ Eq, Order }
import cats.kernel.laws.discipline._
import cats.kernel.Comparison.{ GreaterThan => GT, LessThan => LT, EqualTo => EQ }
import cats.tests.CatsSuite

final class LocationSpec extends CatsSuite {

  import ArbLocation._

  // Laws
  checkAll("Location", OrderTests[Location].order)

  test("Construction should trim trailing min values from Middle") {
    forAll { (l: Location.Middle) =>
      val t = l.toList
      t shouldEqual t.reverse.dropWhile(_ == Int.MinValue).reverse
    }
  }

  test("Construction should produce Beginning if given an empty list") {
    Location.fromFoldable(List.empty[Int]) shouldEqual Location.Beginning
  }

  test("Construction should produce beginning if given all Int.MinValue") {
    forAll { (i: Int) =>
      val count = (i % 10).abs
      val mins  = List.fill(count)(Int.MinValue)
      Location.fromFoldable(mins) shouldEqual Location.Beginning
    }
  }

  test("EQ should agree with ==") {
    forAll { (l0: Location, l1: Location) =>
      Order[Location].eqv(l0, l1) shouldEqual (l0 == l1)
    }
  }

  test("Find should return an empty list if the two positions are the same") {
    forAll { (l: Location) =>
      Location.find(10, l, l) shouldEqual List.empty[Location.Middle]
    }
  }

  test("Find should return an empty list if the first position is >= the second") {
    forAll { (l0: Location, l1: Location) =>
      val res = Order[Location].comparison(l0, l1) match {
        case LT => Location.find(10, l1, l0)
        case _  => Location.find(10, l0, l1)
      }
      res shouldEqual List.empty[Location.Middle]
    }
  }

  test("Find should find nothing if asked for a negative or 0 count") {
    forAll { (i: Int, l0: Location, l1: Location) =>
      val negOrZero = -(i.abs)
      Location.find(negOrZero, l0, l1) shouldEqual List.empty[Location.Middle]
    }
  }

  test("Find should find an arbitrary number of Locations between unequal positions") {
    forAll { (i: Int, l0: Location, l1: Location) =>
      val count = (i % 10000).abs + 1
      Order[Location].comparison(l0, l1) match {
        case LT => Location.find(count, l0, l1).length shouldEqual count
        case GT => Location.find(count, l1, l0).length shouldEqual count
        case EQ => Location.find(count, l0, l1) should have length 0
      }
    }
  }

  test("Find should produce a sorted list of Location.Middle") {
    forAll { (i: Int, l0: Location, l1: Location) =>
      val count = (i % 10000).abs + 1
      val res   = Order[Location].comparison(l0, l1) match {
        case LT => l0 +: Location.find(count, l0, l1).widen[Location] :+ l1
        case _  => l1 +: Location.find(count, l1, l0).widen[Location] :+ l0
      }

      assert(Eq[List[Location]].eqv(res.sorted, res))
    }
  }

  test("Find should grow the position list if necessary") {
    Location.find(1, Location(1, 2), Location(1,3)) match {
      case res :: Nil =>
        res.toList match {
          case 1 :: 2 :: p :: Nil =>
            p should not be Int.MinValue

          case l                  =>
            fail(s"Expectd List(1, 2, p) not ${l.mkString(",")}")
        }

      case l                =>
        fail(s"Expected a single result not ${l.mkString(",")}")
    }
  }

  test("Find should evenly space values it finds") {
    val Max   = BigInt(Int.MaxValue)
    val Min   = BigInt(Int.MinValue)
    val Radix = Max + Min.abs + BigInt(1)

    def toBase10(loc: Location.Middle, len: Int): BigInt = {
      val prefix  = loc.posList.toList
      val posList = prefix ::: List.fill(len - prefix.length)(Int.MinValue)

      posList.foldRight((BigInt(0), BigInt(1))) { case (i, (acc, pow)) =>
        (acc + (BigInt(i) + Min.abs) * pow, pow * Radix)
      }._1
    }

    def positions(locs: List[Location.Middle]): List[BigInt] = {
      val len = locs match {
        case Nil => 0
        case _   => locs.map(_.posList.toList.length).max
      }
      locs.map(toBase10(_, len))
    }

    forAll { (i: Int, l0: Location, l1: Location) =>
      val count      = (i % 10000).abs + 1
      val (fst, snd) = if (l0 < l1) (l0, l1) else (l1, l0)
      val middles    = Location.find(count, fst, snd)
      val maxDiff    = positions(middles.toList) match {
        case Nil | _ :: Nil =>
          BigInt(0)
        case posList        =>
          val gapSizes = posList.zip(posList.drop(1)).map { case (a, b) => b - a }
          gapSizes.max - gapSizes.min
      }

      // The gap between consecutive locations across all the values found
      // should never differ one from the other by more than 1.  That is, the
      // locations should be as evenly spaced as possible.
      maxDiff shouldBe <= (BigInt(1))
    }
  }
}
