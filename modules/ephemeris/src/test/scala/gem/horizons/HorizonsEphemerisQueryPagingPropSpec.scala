// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.EphemerisKey
import gem.enum.Site.GS
import gsp.math.arb.ArbTime._
import gsp.math.syntax.time._

import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Arbitrary.arbitrary

import java.time.{ Duration, Instant }

import scala.math.Ordering.Implicits._
import cats.tests.CatsSuite


class HorizonsEphemerisQueryPagingPropSpec extends CatsSuite {

  import HorizonsEphemerisQueryPagingPropSpec._

  case class TestCase(start: Instant, end: Instant, step: Duration) {

    val paging: List[HorizonsEphemerisQuery] =
      HorizonsEphemerisQuery.paging(titan, GS, start, end, step)
  }

  // Generate reasonable test cases that don't ask for trillions of queries
  implicit val arbTestCase: Arbitrary[TestCase] =
    Arbitrary {
      for {
        s <- arbitrary[Instant]
        d <- arbitrary[Duration].map(_.abs)
        c <- Gen.choose(1, 1000000) // steps
        f <- Gen.choose(0.0, 1.0)
        dʹ = HorizonsEphemerisQuery.MinStepLen max (d / ms.toNanos * ms.toNanos)
        e  = s + dʹ * c.toLong + Duration.ofMillis((dʹ.toMillis * f).round)
      } yield TestCase(s, e, dʹ)
    }

  test("queries should be contiguous") {
    forAll { (t: TestCase) =>
      val times = t.paging.map(q => (q.startTime, q.endTime))

      (times.zip(times.drop(1)).forall { case ((_, e), (s, _)) =>
          e + t.step == s
      }) shouldBe true
    }
  }

  test("last element should be less than two step sizes away from end") {
    forAll { (t: TestCase) =>
      t.paging.lastOption match {
        case None    => fail
        case Some(q) => q.endTime should (be >= t.end and be < (t.end + t.step * 2))
      }
    }
  }

  test("explicitly handle case where last page would have one element using MaxElement-sized pages") {
    // This was a failing randomly generated test case, so we'll leave it as a test.
    val s = Instant.parse("2003-10-24T03:53:56.396546622Z")
    val e = Instant.parse("2003-11-28T00:22:17.151546622Z")
    val d = Duration.parse("PT6.69S")
    val t = TestCase(s, e, d)
    t.paging.lastOption match {
      case None    => fail
      case Some(q) => q.endTime should (be >= t.end and be < (t.end + t.step * 2))
    }
  }

  test("first element should be exactly at start time") {
    forAll { (t: TestCase) =>
      t.paging.headOption match {
        case None    => fail
        case Some(q) => q.startTime shouldEqual t.start
      }
    }
  }

  test("each element is spaced exactly by duration") {
    forAll { (t: TestCase) =>
      val stepMs = t.step.toMillis
      val ds     = t.paging.map(q => (Duration.between(q.startTime, q.endTime).toMillis, q.elementLimit))

      (ds.forall { case (dMs, cnt) =>
        (dMs % stepMs == 0) && ((dMs / stepMs) + 1 == cnt.toLong)
      }) shouldBe true
    }
  }
}

object HorizonsEphemerisQueryPagingPropSpec {

  private val titan = EphemerisKey.MajorBody(606)
  private val ms    = Duration.ofMillis(1)

}
