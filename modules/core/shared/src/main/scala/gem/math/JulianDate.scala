// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{Order, Show}
import cats.implicits._

import java.time.{Instant, LocalDateTime}
import java.time.ZoneOffset.UTC

/** Astronomical time representation of continuous days since noon, November 24,
  * 4714 BC.
  *
  * @param dayNumber      whole Julian Day number
  * @param nanoAdjustment fraction of a day adjustment in nanoseconds
  *
  * @see The Wikipedia [[https://en.wikipedia.org/wiki/Julian_day article]]
  */
sealed abstract case class JulianDate(
  dayNumber:      Int,
  nanoAdjustment: Long
) {

  import JulianDate._

  // Guaranteed by the JulianDate constructors, double checked here.
  assert(dayNumber      >= 0,             s"dayNumber >= 0")
  assert(nanoAdjustment >= MinAdjustment, s"nanoAdjustment >= $MinAdjustment")
  assert(nanoAdjustment <= MaxAdjustment, s"nanoAdjustment <= $MaxAdjustment")


  /** Julian date value as a Double, including Julian Day Number and fractional
    * day since the preceding noon.
    */
  val toDouble: Double =
    dayNumber + nanoAdjustment.toDouble / NanoPerDay.toDouble

  /** Modified Julian Date (MJD) double.  This is logically the same as
    * `toDouble - 2400000.5`. MJD was introduced to preserve a bit of floating
    * point decimal precision in calculations that use Julian dates.  It also
    * makes it easier to directly implement algorithms that work with MJD.
    *
    * @see http://tycho.usno.navy.mil/mjd.html
    */
  def toModifiedDouble: Double = {
    val h = SecondsPerHalfDay.toLong * Billion.toLong
    val d = dayNumber      - 2400000
    val n = nanoAdjustment - h

    val (dʹ,nʹ) = if (n >= MinAdjustment) (d, n)
                  else (d - 1, n + SecondsPerDay.toLong * Billion.toLong)

    dʹ + nʹ.toDouble / NanoPerDay.toDouble
  }
}


object JulianDate {

  /** Seconds per Julian day. */
  val SecondsPerDay: Int =               // 86400
    24 * 60 * 60

  private val SecondsPerHalfDay: Int  =  // 43200
    SecondsPerDay / 2

  private val Billion: Int        = 1000000000
  private val NanoPerDay: Long    = SecondsPerDay.toLong * Billion.toLong

  private val MinAdjustment: Long = -SecondsPerHalfDay.toLong * Billion.toLong
  private val MaxAdjustment: Long =  SecondsPerHalfDay.toLong * Billion.toLong - 1

  /** J2000 reference epoch as Julian Date. */
  val J2000: JulianDate =                // JulianDate(2451545,0)
    JulianDate.ofLocalDateTime(
      LocalDateTime.of(2000, 1, 1, 12, 0, 0)
    )

  /** Convert an `Instant` to a Julian Date.
    */
  def ofInstant(i: Instant): JulianDate =
    ofLocalDateTime(LocalDateTime.ofInstant(i, UTC))

  /** JulianDate from a `LocalDateTime` assumed to represent a time at UTC.
    */
  def ofLocalDateTime(ldt: LocalDateTime): JulianDate = {
    val y   = ldt.getYear
    val m   = ldt.getMonthValue
    val d   = ldt.getDayOfMonth

    // Julian Day Number algorithm from:
    // Fliegel, H.F. and Van Flandern, T.C. (1968). "A Machine Algorithm for
    // Processing Calendar Dates" Communications of the Association of Computing
    // Machines ll, 6sT.

    // Yes, integer division.  -1 for Jan and Feb. 0 for Mar - Dec.
    val t   = (m - 14) / 12

    // Julian Day Number (integer division).
    val jdn = (1461 * (y + 4800 + t))       /  4 +
              ( 367 * (m - 2 - 12 * t))     / 12 -
              (   3 * ((y + 4900 + t)/100)) /  4 +
              d - 32075

    // Whole seconds since midnight
    val secs = ldt.getHour * 3600 + ldt.getMinute * 60 + ldt.getSecond
    val adj  = (secs - SecondsPerHalfDay).toLong * Billion + ldt.getNano

    new JulianDate(jdn, adj) {}
  }

  implicit val JulianDateOrder: Order[JulianDate] =
    Order.by(jd => (jd.dayNumber, jd.nanoAdjustment))

  implicit val JulianDateShow: Show[JulianDate] =
    Show.fromToString

}
