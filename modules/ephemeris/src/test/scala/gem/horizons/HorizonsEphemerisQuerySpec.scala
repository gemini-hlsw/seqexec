// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.EphemerisKey
import gem.enum.Site.GS
import gem.math.Ephemeris
import gem.test.RespectIncludeTags
import gem.test.Tags._
import gem.util.InstantMicros

import cats.implicits._
import cats.tests.CatsSuite

import java.time.{ LocalDateTime, Month }
import java.time.ZoneOffset.UTC
import java.time.temporal.ChronoUnit.DAYS

/** Exercises HorizonsEphemerisQuery.  Because the tests require accessing an
  * external service, they are tagged with "RequiresNetwork" and usually skipped
  * See the build.sbt where RequiresNetwork tests are excluded via the setting:
  *
  *     testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-l", "gem.test.Tags.RequiresNetwork")
  *
  */
@SuppressWarnings(Array("org.wartremover.warts.Equals"))
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

}

object HorizonsEphemerisQuerySpec extends EphemerisTestSupport {
  private val titan = EphemerisKey.MajorBody(606)

  private val start = LocalDateTime.of(2017, Month.AUGUST, 15, 1, 0).toInstant(UTC)
  private val end   = LocalDateTime.of(2017, Month.AUGUST, 15, 2, 0).toInstant(UTC)

  private val startM      = InstantMicros.truncate(start)
  private val endM        = InstantMicros.truncate(end)

  private val startCoords = ephCoords("17:21:29.110300 -21:55:46.509000", "-2.85225", "-1.61124")
  private val endCoords   = ephCoords("17:21:28.904000 -21:55:48.103000", "-2.87880", "-1.58756")

  implicit class QueryOps(q: HorizonsEphemerisQuery) {
    def exec(): Ephemeris =
      Ephemeris.fromFoldable[Vector](q.streamEphemeris.runLog.unsafeRunSync)
  }
}
