// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.math.{ Ephemeris, EphemerisCoordinates }
import gsp.math.syntax.treemap._
import gem.util.Timestamp

import cats.effect.Blocker
import cats.effect.IO
import cats.tests.CatsSuite

import fs2.Stream

import scala.collection.immutable.TreeMap


/** Not really a spec per se, but rather a way to exercise the ephemeris parser
  * with a few fixed examples and get a sense of whether it works.
  */
final class EphemerisParserSpec extends CatsSuite with EphemerisTestSupport {

  test("Must parse") {

    val head = eph(
      "2017-Aug-15 00:00:00.000" -> (("14 50 13.1823 -02 58 45.704", "9.304921", "-12.4881")),
      "2017-Aug-15 00:28:46.800" -> (("14 50 13.4803 -02 58 51.699", "9.338051", "-12.4867")),
      "2017-Aug-15 00:57:33.600" -> (("14 50 13.7794 -02 58 57.693", "9.375816", "-12.4855"))
    )

    val tail = eph(
      "2017-Aug-15 23:01:26.400" -> (("14 50 28.2425 -03 03 34.054", "9.623602", "-12.5227")),
      "2017-Aug-15 23:30:13.200" -> (("14 50 28.5505 -03 03 40.065", "9.646707", "-12.5210")),
      "2017-Aug-15 23:59:00.000" -> (("14 50 28.8594 -03 03 46.075", "9.675139", "-12.5193"))
    )

    checkParse("borrelly", head, tail)
  }

  private def checkParse(
    name: String,
    head: TreeMap[Timestamp, EphemerisCoordinates],
    tail: TreeMap[Timestamp, EphemerisCoordinates]
  ): org.scalatest.Assertion = {

    val e = EphemerisParser.parse(load(name)).option.getOrElse(Ephemeris.empty)

    // This works but the error message isn't helpful when it fails.  There
    // should be a way to combine shouldEqual assertions ...
    assert(
      (e.toMap.size                == 51  ) &&
      (e.toMap.to(head.lastKey)    == head) &&
      (e.toMap.from(tail.firstKey) == tail)
    )
  }

  test("Must stream") {
    val head = eph(
      "2017-Aug-15 00:00:00.000" -> (("14 50 13.1823 -02 58 45.704", "9.304921", "-12.4881")),
      "2017-Aug-15 00:28:46.800" -> (("14 50 13.4803 -02 58 51.699", "9.338051", "-12.4867")),
      "2017-Aug-15 00:57:33.600" -> (("14 50 13.7794 -02 58 57.693", "9.375816", "-12.4855"))
    )

    val s = Blocker[IO].use(stream("borrelly", _).through(EphemerisParser.elements[IO]).take(head.size.toLong).compile.toVector)
    val m = TreeMap.fromFoldable(s.unsafeRunSync)

    assert(m == head)
  }

  test("Must handle errors") {
    val z = Timestamp.Min -> EphemerisCoordinates.Zero
    val s = Blocker[IO].use(stream("borrelly-error", _)
             .through(EphemerisParser.elements[IO])
             .handleErrorWith(_ => Stream(z)).last.compile.toVector)
    assert(Vector(Some(z)) == s.unsafeRunSync)
  }

  test("Must stream eitherElements") {
    val head = Vector[Either[String, Ephemeris.Element]](
      Right(time("2017-Aug-15 00:00:00.000") -> ephCoords("14 50 13.1823 -02 58 45.704", "9.304921", "-12.4881")),
      Right(time("2017-Aug-15 00:28:46.800") -> ephCoords("14 50 13.4803 -02 58 51.699", "9.338051", "-12.4867")),
      Left("Failure reading:solarPresence")
    )

    val s = Blocker[IO].use(stream("borrelly-error", _).through(EphemerisParser.eitherElements[IO]).take(head.size.toLong).compile.toVector)
    val m = s.unsafeRunSync()

    assert(m == head)
  }

  test("Must stream validElements") {
    val head = eph(
      "2017-Aug-15 00:00:00.000" -> (("14 50 13.1823 -02 58 45.704", "9.304921", "-12.4881")), // 0
      "2017-Aug-15 00:28:46.800" -> (("14 50 13.4803 -02 58 51.699", "9.338051", "-12.4867")), // 1
      "2017-Aug-15 01:26:20.400" -> (("14 50 14.0798 -02 59 03.686", "9.417733", "-12.4845"))  // 3 (skipping 2)
    )

    val s = Blocker[IO].use(stream("borrelly-error", _).through(EphemerisParser.validElements[IO]).take(head.size.toLong).compile.toVector)
    val m = TreeMap.fromFoldable(s.unsafeRunSync)

    assert(m == head)
  }

}
