// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.EphemerisKey
import gem.enum.Site.GN
import gem.math.Ephemeris
import gem.test.RespectIncludeTags
import gem.test.Tags._

import EphemerisCompression._

import cats.effect.Blocker
import cats.effect.IO
import cats.implicits._
import cats.tests.CatsSuite

import fs2.Pipe

import java.time.{ LocalDate, Month }
import java.time.temporal.ChronoUnit.MINUTES

final class EphemerisCompressionSpec extends CatsSuite with EphemerisTestSupport with RespectIncludeTags {
  import EphemerisCompressionSpec._

  test("compresses") {
    val a = Blocker[IO].use(stream("borrelly", _)
              .through(EphemerisParser.elements[IO])
              .through(standardVelocityCompression).compile.toVector)

    val e = Blocker[IO].use(stream("borrelly-compressed", _)
              .through(EphemerisParser.elements[IO]).compile.toVector)

    val actual   = a.unsafeRunSync
    val expected = e.unsafeRunSync

    assert(actual == expected)
  }

  val Δv: Pipe[IO, Ephemeris.Element, Ephemeris.Element] = standardVelocityCompression
  val ac: Pipe[IO, Ephemeris.Element, Ephemeris.Element] = standardAccelerationCompression

  test("2014 UR Δv compresses to 100% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.SEPTEMBER, 15)
    val end   = LocalDate.of(2015, Month.NOVEMBER,  15)

    testCompression("2014 UR Δv", ur_2014, 1.00, start, end, Δv)
  }

  test("2014 UR a compresses to 10.1% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.SEPTEMBER, 15)
    val end   = LocalDate.of(2015, Month.NOVEMBER,  15)

    testCompression("2014 UR a", ur_2014, 0.101, start, end, ac)
  }

  test("2015 QT3 Δv compresses to 34% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.AUGUST,  1)
    val end   = LocalDate.of(2015, Month.OCTOBER, 1)

    testCompression("2015 QT3 Δv", qt3_2015, 0.342, start, end, Δv)
  }

  test("2015 QT3 a compresses to 5.6% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.AUGUST,  1)
    val end   = LocalDate.of(2015, Month.OCTOBER, 1)

    testCompression("2015 QT3 a", qt3_2015, 0.056, start, end, ac)
  }

  test("Churyumov-Gerasimenko Δv compresses to 4% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.DECEMBER, 15)
    val end   = LocalDate.of(2016, Month.FEBRUARY, 15)

    testCompression("Churyumov Δv", churyumov, 0.039, start, end, Δv)
  }

  test("Churyumov-Gerasimenko a compresses to 1.2% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.DECEMBER, 15)
    val end   = LocalDate.of(2016, Month.FEBRUARY, 15)

    testCompression("Churyumov a", churyumov, 0.012, start, end, ac)
  }

  test("Titan Δv compresses to 1% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.JULY  ,    1)
    val end   = LocalDate.of(2015, Month.SEPTEMBER, 1)

    testCompression("Titan Δv", titan, 0.008, start, end, Δv)
  }

  test("Titan a compresses to 0.6% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.JULY  ,    1)
    val end   = LocalDate.of(2015, Month.SEPTEMBER, 1)

    testCompression("Titan a", titan, 0.006, start, end, ac)
  }

  test("Beer Δv compresses to 5% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.OCTOBER,  1)
    val end   = LocalDate.of(2015, Month.DECEMBER, 1)

    testCompression("Beer Δv", beer, 0.046, start, end, Δv)
  }

  test("Beer a compresses to 1.4% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.OCTOBER,  1)
    val end   = LocalDate.of(2015, Month.DECEMBER, 1)

    testCompression("Beer a", beer, 0.014, start, end, ac)
  }

  test("Io Δv compresses to 24% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.MARCH, 1)
    val end   = LocalDate.of(2015, Month.MAY,   1)

    testCompression("Io Δv", io, 0.237, start, end, Δv)
  }

  test("Io a compresses to 3.2% of original size", RequiresNetwork) {
    val start = LocalDate.of(2015, Month.MARCH, 1)
    val end   = LocalDate.of(2015, Month.MAY,   1)

    testCompression("Io a", io, 0.032, start, end, ac)
  }

  def testCompression(
        name:          String,
        key:           EphemerisKey.Horizons,
        expectedRatio: Double,
        start:         LocalDate,
        end:           LocalDate,
        pipe:          Pipe[IO, Ephemeris.Element, Ephemeris.Element]): org.scalatest.Assertion = {

    // Start and end at midnight local time.
    val zstart = start.atTime(0, 0).atZone(GN.timezone)
    val zend   = end.atTime(0, 0).atZone(GN.timezone)

    // One element per minute
    val elems  = zstart.until(zend, MINUTES).toInt

    val e      = HorizonsEphemerisQuery(key, GN, zstart.toInstant, zend.toInstant, elems).exec(pipe)
    val size   = e.toMap.size
    val ratio  = size.toDouble / elems.toDouble

    println(f"$name%-12s $key%-21s => $size%5d / $elems%5d elements, ${ratio * 100.0}%5.2f%% of original")

    assert((ratio >= expectedRatio - 0.001) &&
           (ratio <= expectedRatio + 0.001))
  }
}

object EphemerisCompressionSpec {
  private val beer      = EphemerisKey.AsteroidNew("1971 UC1")
  private val churyumov = EphemerisKey.Comet("67P")
  private val io        = EphemerisKey.MajorBody(501)
  private val titan     = EphemerisKey.MajorBody(606)
  private val qt3_2015  = EphemerisKey.AsteroidNew("2015 QT3")
  private val ur_2014   = EphemerisKey.AsteroidNew("2014 UR")

  implicit class QueryOps(q: HorizonsEphemerisQuery) {
    def exec(compression: Pipe[IO, Ephemeris.Element, Ephemeris.Element]): Ephemeris = {
      val s = q.streamEphemeris.through(compression)
      Ephemeris.fromFoldable[Vector](s.compile.toVector.unsafeRunSync)
    }
  }
}
