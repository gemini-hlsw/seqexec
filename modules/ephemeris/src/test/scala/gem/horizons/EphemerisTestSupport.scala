// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.math.Coordinates
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

  def eph(elems: (String, String)*): TreeMap[InstantMicros, Coordinates] =
    TreeMap(elems.map { case (i, c) => time(i) -> coords(c) }: _*)
}
