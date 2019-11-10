// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.math.EphemerisCoordinates
import gsp.math.{ Angle, Coordinates, Offset }
import gsp.math.syntax.treemap._
import gem.util.Timestamp

import cats.effect.{ Blocker, IO, ContextShift }
import fs2.Stream

import java.io.InputStream
import java.time.{ LocalDateTime, ZoneOffset }
import java.time.format.DateTimeFormatter

import scala.collection.immutable.TreeMap
import scala.io.Source


trait EphemerisTestSupport {

  private implicit val contextShift: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.global)

  val TimeFormat: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MMM-dd HH:mm:ss.SSS")

  def time(s: String): Timestamp =
    Timestamp.unsafeFromInstant(LocalDateTime.parse(s, TimeFormat).toInstant(ZoneOffset.UTC))

  def coords(s: String): Coordinates =
    Coordinates.fromHmsDms.getOption(s).getOrElse(Coordinates.Zero)

  def arcsec(s: String): Angle =
    Angle.fromMicroarcseconds(BigDecimal(s).underlying.movePointRight(6).longValueExact)

  def offsetp(s: String): Offset.P =
    Offset.P(arcsec(s))

  def offsetq(s: String): Offset.Q =
    Offset.Q(arcsec(s))

  def ephCoords(c: String, p: String, q: String): EphemerisCoordinates =
    EphemerisCoordinates(coords(c), Offset(offsetp(p), offsetq(q)))

  def eph(elems: (String, (String, String, String))*): TreeMap[Timestamp, EphemerisCoordinates] =
    TreeMap.fromList(elems.toList.map { case (i, (c, p, q)) => time(i) -> ephCoords(c, p, q) })

  def inputStream(n: String): InputStream =
    getClass.getResourceAsStream(s"$n.eph")

  def stream(n: String, b: Blocker): Stream[IO, String] =
    fs2.io.readInputStream(IO(inputStream(n)), 128, b)
          .through(fs2.text.utf8Decode)

  def load(n: String): String =
    Source.fromInputStream(inputStream(n)).mkString
}
