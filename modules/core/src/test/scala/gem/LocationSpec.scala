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

  it should "produce a sorted list of Location.Middle" in {
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

  private def check(count: Int, low: Location, hi: Location, expected: List[Int]*): org.scalatest.compatible.Assertion =
    Location.find(count, low, hi).toList.map(_.toList) shouldEqual expected.toList

  it should "evenly space values it finds" in {
    check(2, Location( 0), Location(10),  3 :: Nil,  6 :: Nil)
    check(2, Location( 0), Location( 9),  3 :: Nil,  6 :: Nil)
    check(2, Location( 0), Location( 8),  3 :: Nil,  6 :: Nil)
    check(2, Location( 0), Location( 7),  2 :: Nil,  4 :: Nil)
    check(2, Location( 0), Location( 6),  2 :: Nil,  4 :: Nil)
    check(2, Location( 0), Location( 5),  2 :: Nil,  4 :: Nil)
    check(2, Location( 0), Location( 4),  1 :: Nil,  2 :: Nil)
    check(2, Location( 0), Location( 3),  1 :: Nil,  2 :: Nil)
    check(2, Location(-7), Location( 0), -5 :: Nil, -3 :: Nil)

    check(1, Location(1, 2), Location(1, 3),  1 :: 2 :: 0 :: Nil)
    check(2, Location(1, 2), Location(1, 3),  1 :: 2 :: -715827883 :: Nil, 1 :: 2 :: 715827882 :: Nil)

    check(2, Location(0), Location(2), 0 :: 715827883 :: Nil, 1 :: -715827882 :: Nil)

    check(1, Location.Beginning,         Location.End,               -1 :: Nil)
    check(1, Location.Beginning,         Location(Int.MinValue + 2), Int.MinValue + 1 :: Nil)
    check(1, Location(Int.MaxValue - 2), Location.End,               Int.MaxValue - 1 :: Nil)
  }
}
