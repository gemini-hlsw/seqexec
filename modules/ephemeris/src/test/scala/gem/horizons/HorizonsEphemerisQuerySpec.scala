// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.EphemerisKey
import gem.enum.Site.GS
import gem.math.Ephemeris
import gem.test.RespectIncludeTags
import gem.test.Tags._
import gem.util.Timestamp
import gsp.math.syntax.time._

import cats.implicits._
import cats.tests.CatsSuite

import java.time.{ Duration, Instant, LocalDateTime, Month }
import java.time.ZoneOffset.UTC
import java.time.temporal.ChronoUnit.DAYS

/** Exercises HorizonsEphemerisQuery.  Because the tests require accessing an
  * external service, they are tagged with "RequiresNetwork" and usually skipped
  * See the build.sbt where RequiresNetwork tests are excluded via the setting:
  *
  *     testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-l", "gem.test.Tags.RequiresNetwork")
  *
  */
final class HorizonsEphemerisQuerySpec extends CatsSuite with EphemerisTestSupport with RespectIncludeTags {

  import HorizonsEphemerisQuerySpec._

  test("simple query should work", RequiresNetwork) {
    val e = HorizonsEphemerisQuery(titan, GS, start, end, 60).exec()

    assert(
      e.first.exists(_ == (startM -> startCoords)) &&
      e.last .exists(_ == (endM   -> endCoords  )) &&
      e.toMap.size == 60
    )
  }

  test("at least 2 values are returned (start and end)", RequiresNetwork) {
    val e = HorizonsEphemerisQuery(titan, GS, start, end, -1).exec()

    assert(
      e.first.exists(_ == (startM -> startCoords)) &&
      e.last .exists(_ == (endM   -> endCoords  )) &&
      e.toMap.size == 2
    )
  }

  test("caps requests at MaxElements", RequiresNetwork, Slow) {
    val e = HorizonsEphemerisQuery(titan, GS, start, end.plus(182, DAYS), HorizonsEphemerisQuery.MaxElements + 1).exec()

    assert(e.toMap.size == HorizonsEphemerisQuery.MaxElements)
  }

  test("returns nothing if end comes before start", RequiresNetwork) {
    val e = HorizonsEphemerisQuery(titan, GS, end, start, 60).exec()

    assert(e.toMap.size == 0)
  }

  private def checkPaging(start: Instant, end: Instant, expected: HorizonsEphemerisQuery*): org.scalatest.Assertion = {
    val as = HorizonsEphemerisQuery.paging(titan, GS, start, end, OneMinute)
    val es = expected.toList

    assert(es.size == as.size && es.zip(as).forall { case (e, a) =>
      e.startTime    == a.startTime    &&
      e.endTime      == a.endTime      &&
      e.elementLimit == a.elementLimit
    })
  }

  val Max: Int = HorizonsEphemerisQuery.MaxElements

  test("paging a negative time amount should be empty") {
    checkPaging(end, start)
  }

  test("paging a zero time amount should be empty") {
    checkPaging(start, start)
  }

  test("paging less time than the step size should produce two elements, one query") {
    checkPaging(
      start,
      start + Duration.ofNanos(1),
      HorizonsEphemerisQuery(titan, GS, start, start + OneMinute, 2)
    )
  }

  test("paging a time that requires < MaxElements should result in one query") {
    checkPaging(
      start,
      end,
      HorizonsEphemerisQuery(titan, GS, start, end, 61)
    )
  }

  test("paging exactly MaxElements should result in a single query") {
    val end = start + OneMinute * (Max.toLong - 1L) // remember *inclusive*

    checkPaging(
      start,
      end,
      HorizonsEphemerisQuery(titan, GS, start, end, Max)
    )
  }

  test("paging MaxElements + 1 should result in two queries") {

    // Produces Max + 1 elements because end is inclusive.
    val end = start + OneMinute * Max.toLong

    checkPaging(
      start,
      end,
      // Here the first query has Max elements.  The second query would have
      // just one element but since that won't work with horizons it is extended
      // to include an additional element.
      HorizonsEphemerisQuery(titan, GS, start, end - OneMinute, Max),
      HorizonsEphemerisQuery(titan, GS, end,   end + OneMinute,   2)
    )
  }

  test("paging MaxElements + 2 should result in two queries") {

    // Produces Max + 2 elements because end is inclusive
    val end = start + OneMinute * (Max + 1).toLong

    checkPaging(
      start,
      end,
      // The first query can contain a full MaxElements page because the second
      // query will pick up the remaining two.
      HorizonsEphemerisQuery(titan, GS, start,           end - OneMinute * 2L, Max),
      HorizonsEphemerisQuery(titan, GS, end - OneMinute, end,                  2  )
    )
  }

  test("paging MaxElements + 3 should result in two queries") {

    // Produces Max + 3 elements because end is inclusive
    val end = start + OneMinute * (Max + 2).toLong

    checkPaging(
      start,
      end,
      // The first query can contain a full MaxElements page because the second
      // query will pick up the remaining three.
      HorizonsEphemerisQuery(titan, GS, start,                end - OneMinute * 3L, Max),
      HorizonsEphemerisQuery(titan, GS, end - OneMinute * 2L, end,                  3  )
    )
  }
}

object HorizonsEphemerisQuerySpec extends EphemerisTestSupport {
  private val titan = EphemerisKey.MajorBody(606)

  private val start = LocalDateTime.of(2017, Month.AUGUST, 15, 1, 0).toInstant(UTC)
  private val end   = LocalDateTime.of(2017, Month.AUGUST, 15, 2, 0).toInstant(UTC)

  private val startM      = Timestamp.unsafeFromInstant(start)
  private val endM        = Timestamp.unsafeFromInstant(end)

  private val startCoords = ephCoords("17:21:29.110300 -21:55:46.509000", "-2.85225", "-1.61124")
  private val endCoords   = ephCoords("17:21:28.904000 -21:55:48.103000", "-2.87880", "-1.58756")

  private val OneMinute   = Duration.ofMinutes(1)

  implicit class QueryOps(q: HorizonsEphemerisQuery) {
    def exec(): Ephemeris =
      Ephemeris.fromFoldable[Vector](q.streamEphemeris.compile.toVector.unsafeRunSync)
  }
}
