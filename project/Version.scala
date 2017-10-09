// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

import java.time._
import scala.math.floor
import scala.sys.process._

// Version based on J2000 epoch
object Version {

  // The short hash for the last commit, like d68549f
  def unsafeHeadCommit: String =
    "git rev-parse --short HEAD".!!.trim

  // True if there are uncommitted local changes
  def unsafeLocalchanges: Boolean =
    "git update-index -q --refresh".! == 0

  // Unix epoch time in SECONDS for the last commit, like 1507570492
  def unsafeUnixSecond: Long =
    s"git show -s --pretty=format:%ct HEAD".!!.trim.toLong

  // Instant UTC for the last commit
  def unsafeInstant: Instant =
    Instant.ofEpochSecond(unsafeUnixSecond)

  // LocalDateTime for the last commit
  def unsafeLocalDateTime: LocalDateTime =
    LocalDateTime.ofInstant(unsafeInstant, ZoneOffset.UTC)

  // Fractional Julian day year for the last commit
  def unsafeJulianDay: Double = {
    val dt = unsafeLocalDateTime
    val a = floor((14.0 - dt.getMonthValue) / 12.0)
    val y = dt.getYear + 4800.0 - a
    val m = dt.getMonthValue + 12 * a - 3.0
    dt.getDayOfMonth +
    floor((153.0 * m + 2.0) / 5.0) +
    365 * y +
    floor(y / 4.0) -
    floor(y / 100.0) +
    floor(y / 400.0) -
    32045.0
  }

  // Fractional J2000 Epoch year for the last commit, like 2017.123456
  def unsafeJ2000: Double = {
    val yearBasis:    Double = 2000.0
    val julianBasis:  Double = 2451545.0
    val lengthOfYear: Double = 365.25
    yearBasis + (unsafeJulianDay - julianBasis) / lengthOfYear
  }

  // Current version
  def unsafeCurrent: String = {
    val h = unsafeHeadCommit
    val y = unsafeJ2000
    val c = if (unsafeLocalchanges) "+" else ""
    f"$y%8.3f-$h$c"
  }

}
