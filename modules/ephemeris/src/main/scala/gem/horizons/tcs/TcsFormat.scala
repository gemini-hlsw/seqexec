// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons.tcs

import gem.math.Ephemeris
import gsp.math.{ Angle, Declination, HourAngle, JulianDate, RightAscension }

import cats._

import fs2._

import java.time.ZoneOffset.UTC
import java.time.format.DateTimeFormatter
import java.util.Locale.US

/** Formatting for TCS ephemeris files.  The TCS is very particular and
  * unforgiving about the format, down to the column in which items start and
  * the number of digits to the right of decimal places.
  */
object TcsFormat {

  /** Exact header required by the TCS. */
  val Header: String =
    """***************************************************************************************
      | Date__(UT)__HR:MN Date_________JDUT     R.A.___(ICRF/J2000.0)___DEC dRA*cosD d(DEC)/dt
      |***************************************************************************************""".stripMargin

  /** Marker for the start of ephemeris elements in the file. */
  val Soe: String =
    "$$SOE"

  /** Marker for the end of ephemeris elements in the file. */
  val Eoe: String =
    "$$EOE"

  /** Date format required by the TCS. */
  val DateFormatPattern: String      =
    "yyyy-MMM-dd HH:mm"

  val DateFormat: DateTimeFormatter =
    DateTimeFormatter.ofPattern(DateFormatPattern, US).withZone(UTC)

  /** RA format, which requires 10th of a millisecond (and no more, and no less)
    * precision.
    */
  def formatRa(ra: RightAscension): String = {
    // Round to an even number of tenths of microseconds.
    val µs         = ra.toHourAngle.toMicroseconds
    val tenthsOfMs = (µs / 100) + ((µs % 100) + 50) / 100
    val hms        = (HourAngle.microseconds.reverse composeIso HourAngle.hms).get(tenthsOfMs * 100)

    f"${hms.hours}%02d ${hms.minutes}%02d ${hms.seconds}%02d.${hms.milliseconds}%03d${hms.microseconds/100}%01d"
  }

  /** Dec format, which requires millisecond (and no more, and no less)
    * precision.  If positive, must have a leading space.
    */
  def formatDec(dec: Declination): String = {
    val a    = dec.toAngle
    val sµas = Angle.signedMicroarcseconds.get(a)

    // Round to an even number of milliseconds
    val µas  = (if (sµas < 0) -a else a).toMicroarcseconds
    val mas  = (µas / 1000) + ((µas % 1000) + 500) / 1000
    val dms  = (Angle.milliarcseconds.reverse composeIso Angle.dms).get(mas.toInt)

    // -9 should format as "-09", 9 as " 09"
    val sign = if (sµas < 0) "-" else " "
    f"$sign${dms.degrees.abs}%02d ${dms.arcminutes}%02d ${dms.arcseconds}%02d.${dms.milliarcseconds}%03d"
  }

  def formatDeltaArcseconds(a: Angle): String =
    f"${Angle.signedMicroarcseconds.get(a).toDouble / 1000000.0}%9.5f"

  def formatElement(e: Ephemeris.Element): String = {
    val (time, ephCoords) = e

    // Time, Julian Date
    val instant   = time.toInstant
    val timeS     = DateFormat.format(instant)
    val jdS       = f"${JulianDate.ofInstant(instant).toDouble}%.9f"

    // Coordinates
    val coords = ephCoords.coord
    val coordS = s"${formatRa(coords.ra)} ${formatDec(coords.dec)}"
    val Δ      = ephCoords.delta
    val ΔraS   = formatDeltaArcseconds(Δ.p.toAngle)
    val ΔdecS  = formatDeltaArcseconds(Δ.q.toAngle)

    s" $timeS $jdS     $coordS $ΔraS $ΔdecS"
  }

  def elements[M[_]: Monad]: Pipe[M, Ephemeris.Element, String] =
    _.map(formatElement)

  def ephemeris[M[_]: Monad]: Pipe[M, Ephemeris.Element, String] =
    s => Stream(Header, Soe) ++ s.map(formatElement) ++ Stream(Eoe)
}