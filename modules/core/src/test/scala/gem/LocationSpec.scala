// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz._, Scalaz._
import scalaz.Ordering._

@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.NonUnitStatements"))
class LocationSpec extends FlatSpec with Matchers with PropertyChecks with Arbitraries {
  "Construction" should "trim trailing min values from Middle" in {
    forAll { (l: Location.Middle) =>
      val t = l.toIList
      t shouldEqual t.dropRightWhile(_ == Int.MinValue)
    }
  }

  it should "produce Beginning if given an empty list" in {
    Location.fromFoldable(List.empty[Int]) shouldEqual Location.Beginning
  }

  it should "produce beginning if given all Int.MinValue" in {
    forAll { (i: Int) =>
      val count = (i % 10).abs
      val mins  = List.fill(count)(Int.MinValue)
      Location.fromFoldable(mins) shouldEqual Location.Beginning
    }
  }

  "EQ" should "agree with ==" in {
    forAll { (l0: Location, l1: Location) =>
      Order[Location].equal(l0, l1) shouldEqual (l0 == l1)
    }
  }

  "Find" should "return an empty list if the two positions are the same" in {
    forAll { (l: Location) =>
      Location.find(10, l, l) shouldEqual IList.empty[Location.Middle]
    }
  }

  it should "return an empty list if the first position is >= the second" in {
    forAll { (l0: Location, l1: Location) =>
      val res = Order[Location].order(l0, l1) match {
        case LT => Location.find(10, l1, l0)
        case _  => Location.find(10, l0, l1)
      }
      res shouldEqual IList.empty[Location.Middle]
    }
  }

  it should "find nothing if asked for a negative or 0 count" in {
    forAll { (i: Int, l0: Location, l1: Location) =>
      val negOrZero = -(i.abs)
      Location.find(negOrZero, l0, l1) shouldEqual IList.empty[Location.Middle]
    }
  }

  it should "find an arbitrary number of Locations between unequal positions" in {
    forAll { (i: Int, l0: Location, l1: Location) =>
      val count = (i % 10000).abs + 1
      Order[Location].order(l0, l1) match {
        case LT => Location.find(count, l0, l1).length shouldEqual count
        case GT => Location.find(count, l1, l0).length shouldEqual count
        case EQ => Location.find(count, l0, l1) should have length 0
      }
    }
  }

  ignore should "produce a sorted list of Location.Middle" in {
    forAll { (i: Int, l0: Location, l1: Location) =>
      val count = (i % 10000).abs + 1
      val res   = Order[Location].order(l0, l1) match {
        case LT => l0 +: Location.find(count, l0, l1).widen[Location] :+ l1
        case _  => l1 +: Location.find(count, l1, l0).widen[Location] :+ l0
      }
      res.sorted shouldEqual res
    }
  }

  it should "grow the position list if necessary" in {
    Location.find(1, Location(1, 2), Location(1,3)) match {
      case ICons(res, INil()) =>
        res.toList match {
          case 1 :: 2 :: p :: Nil =>
            p should not be Int.MinValue

          case l                  =>
            fail(s"Expectd List(1, 2, p) not ${l.mkString(",")}")
        }

      case l                =>
        fail(s"Expected a single result not ${l.toList.mkString(",")}")
    }
  }

  it should "evenly space values it finds" in {
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
        case _   => locs.map(_.posList.length).max
      }
      locs.map(toBase10(_, len))
    }

    forAll { (i: Int, l0: Location, l1: Location) =>
      val count      = (i % 10000).abs + 1
      val (fst, snd) = Order[Location].sort(l0, l1)
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
