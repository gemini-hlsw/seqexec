// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.EphemerisKey
import gem.enum.Site.GN
import gem.math.Ephemeris
import gem.test.RespectIncludeTags
import gem.test.Tags._

import VelocityCompression._

import cats.effect.IO
import cats.implicits._
import cats.tests.CatsSuite

import java.time.{ LocalDate, Month }
import java.time.temporal.ChronoUnit.MINUTES

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
final class VelocityCompressionSpec extends CatsSuite with EphemerisTestSupport with RespectIncludeTags {
  import VelocityCompressionSpec._

  test("compresses") {
    val a = stream("borrelly")
              .through(EphemerisParser.elements[IO])
              .through(standardVelocityCompression)

    val e = stream("borrelly-compressed")
              .through(EphemerisParser.elements[IO])

    val actual   = a.runLog.unsafeRunSync
    val expected = e.runLog.unsafeRunSync

    assert(actual == expected)
  }

  // Andy's result: 99%
  test("2014 UR compresses to 79% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.SEPTEMBER, 15)
    val end   = LocalDate.of(2015, Month.NOVEMBER,  15)

    testCompression("2014 UR", ur_2014, 0.79, start, end)
  }

  // Andy's result: 34%
  test("2015 QT3 compresses to 33% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.AUGUST,  1)
    val end   = LocalDate.of(2015, Month.OCTOBER, 1)

    testCompression("2015 QT3", qt3_2015, 0.33, start, end)
  }

  // Andy's result: 4%
  test("Churyumov-Gerasimenko compresses to 3% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.DECEMBER, 15)
    val end   = LocalDate.of(2016, Month.FEBRUARY, 15)

    testCompression("Churyumov", churyumov, 0.03, start, end)
  }

  test("Titan compresses to 1% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.JULY  ,    1)
    val end   = LocalDate.of(2015, Month.SEPTEMBER, 1)

    testCompression("Titan", titan, 0.01, start, end)
  }

  // Andy's result: 5%
  test("Beer compresses to 4% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.OCTOBER,  1)
    val end   = LocalDate.of(2015, Month.DECEMBER, 1)

    testCompression("Beer", beer, 0.04, start, end)
  }

  // Andy's result: 24%
  test("Io compresses to 23% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.MARCH, 1)
    val end   = LocalDate.of(2015, Month.MAY,   1)

    testCompression("Io", io, 0.23, start, end)
  }

  def testCompression(
        name:          String,
        key:           EphemerisKey.Horizons,
        expectedRatio: Double,
        start:         LocalDate,
        end:           LocalDate): org.scalatest.Assertion = {

    // Start and end at midnight local time.
    val zstart = start.atTime(0, 0).atZone(GN.timezone)
    val zend   = end.atTime(0, 0).atZone(GN.timezone)

    // One element per minute
    val elems  = zstart.until(zend, MINUTES).toInt

    val e      = HorizonsEphemerisQuery(key, GN, zstart.toInstant, zend.toInstant, elems).exec()
    val size   = e.toMap.size
    val ratio  = size.toDouble / elems.toDouble

    println(f"$name%-10s $key%-21s => $size%5d / $elems%5d elements, ${ratio * 100.0}%5.2f%% of original")

    assert((ratio >= expectedRatio - 0.01) &&
           (ratio <= expectedRatio + 0.01))
  }
}

object VelocityCompressionSpec {
  private val beer      = EphemerisKey.AsteroidNew("1971 UC1")
  private val churyumov = EphemerisKey.Comet("67P")
  private val io        = EphemerisKey.MajorBody(501)
  private val titan     = EphemerisKey.MajorBody(606)
  private val qt3_2015  = EphemerisKey.AsteroidNew("2015 QT3")
  private val ur_2014   = EphemerisKey.AsteroidNew("2014 UR")

  implicit class QueryOps(q: HorizonsEphemerisQuery) {
    import VelocityCompression._

    def exec(): Ephemeris = {
      val s = q.streamEphemeris.through(standardVelocityCompression)
      Ephemeris.fromFoldable[Vector](s.runLog.unsafeRunSync)
    }
  }
}
