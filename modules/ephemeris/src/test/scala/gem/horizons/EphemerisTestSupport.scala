// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.math.{ Angle, EphemerisCoordinates, Coordinates, Offset }
import gem.util.InstantMicros

import java.time.{LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter

import scala.collection.immutable.TreeMap

trait EphemerisTestSupport {
  val TimeFormat: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MMM-dd HH:mm:ss.SSS")

  def time(s: String): InstantMicros =
    InstantMicros.truncate(LocalDateTime.parse(s, TimeFormat).toInstant(ZoneOffset.UTC))

  def coords(s: String): Coordinates =
    Coordinates.parse(s).getOrElse(Coordinates.Zero)

  def arcsec(s: String): Angle =
    Angle.fromMicroarcseconds(BigDecimal(s).underlying.movePointRight(6).longValueExact)

  def offsetp(s: String): Offset.P =
    Offset.P(arcsec(s))

  def offsetq(s: String): Offset.Q =
    Offset.Q(arcsec(s))

  def ephCoords(c: String, p: String, q: String): EphemerisCoordinates =
    EphemerisCoordinates(coords(c), Offset(offsetp(p), offsetq(q)))

  def eph(elems: (String, (String, String, String))*): TreeMap[InstantMicros, EphemerisCoordinates] =
    TreeMap(elems.map { case (i, (c, p, q)) => time(i) -> ephCoords(c, p, q) }: _*)
}
