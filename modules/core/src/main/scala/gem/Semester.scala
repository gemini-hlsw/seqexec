// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import atto._, Atto._
import gem.enum.{ Half, Site }
import java.time._
import java.time.Month._
import scalaz.Order, scalaz.syntax.semigroup._

/** A (Year, Half) pair. */
final case class Semester(year: Year, half: Half) {

  /** This Semester plus the given number of years. */
  def plusYears(n: Int): Semester =
    copy(year = year.plusYears(n.toLong))

  /** This Semester plus the given number of half-years. */
  def plusSemesters(n: Int): Semester = {
    val yy = year.getValue
    val hs = yy * 2 + half.toInt * yy.signum + n
    Semester(Year.of(hs / 2), Half.unsafeFromInt(hs % 2))
  }

  /** The semester immediately following this one. */
  def next: Semester =
    plusSemesters(1)

  /** The semester immediately preceding this one. */
  def prev: Semester =
    plusSemesters(-1)

  /** Module of various representations of the semester start. */
  object start {
    lazy val yearMonth: YearMonth = YearMonth.of(year.getValue, half.startMonth)
    lazy val localDate: LocalDate = LocalDate.of(year.getValue, half.startMonth, 1)
    lazy val localDateTime: LocalDateTime = LocalDateTime.of(localDate, LocalTime.MIDNIGHT).minusHours(10) // 2pm the day before
    def zonedDateTime(zoneId: ZoneId): ZonedDateTime = ZonedDateTime.of(localDateTime, zoneId)
    def atSite(site: Site): ZonedDateTime = zonedDateTime(site.timezone)
  }

  /** Module of various representations of the semester end. */
  object end {
    lazy val yearMonth: YearMonth = YearMonth.of(year.getValue, half.endMonth)
    lazy val localDate: LocalDate = LocalDate.of(year.getValue, half.endMonth, half.endMonth.maxLength)
    lazy val localDateTime: LocalDateTime = LocalDateTime.of(localDate, LocalTime.MIDNIGHT).plusHours(14) // 2pm today
    def zonedDateTime(zoneId: ZoneId): ZonedDateTime = ZonedDateTime.of(localDateTime, zoneId)
    def atSite(site: Site): ZonedDateTime = zonedDateTime(site.timezone)
  }

  /** Format as full year and semester half, `2009A`. */
  def format: String =
    s"$year$half"

  /** Format as 2-digit year and semester half, `09A`. */
  def formatShort: String =
    f"${year.getValue % 100}%02d$half"

  override def toString =
    f"Semester($format)"

}

object Semester {

  /** Semester for the specified year and month. */
  def fromYearMonth(ym: YearMonth): Semester = {
    val m = ym.getMonth
    val y = m match {
      case JANUARY => ym.getYear - 1
      case _       => ym.getYear
    }
    Semester(Year.of(y), Half.fromMonth(m))
  }

  /** Semester for the specified year and month. The day of month is ignored. */
  def fromLocalDate(d: LocalDate): Semester =
    fromYearMonth(YearMonth.of(d.getYear, d.getMonth))

  /**
   * Semester for the specified local date and time. This handles the 2pm switchover on the last
   * day of the semester by adding 10 hours to the given date-time, then looking up by month.
   */
  def fromLocalDateTime(d: LocalDateTime): Semester = {
    val dʹ = d.plusHours(10)
    fromYearMonth(YearMonth.of(dʹ.getYear, dʹ.getMonth))
  }

  /**
   * Semester for the specified zoned date and time. This handles the 2pm switchover on the last
   * day of the semester by adding 10 hours to the given date-time, then looking up by month.
   */
  def fromZonedDateTime(d: ZonedDateTime): Semester =
    fromLocalDateTime(d.toLocalDateTime)

  /** Semester for the zoned date and time of the given Site and Instant. */
  def fromSiteAndInstant(s: Site, i: Instant): Semester =
    fromZonedDateTime(ZonedDateTime.ofInstant(i, s.timezone))

  /** Parse a full-year Semester like `2009A` from a String, if possible. */
  def fromString(s: String): Option[Semester] =
    (Parsers.semester <~ endOfInput).parseOnly(s).option

  /** Parse a full-year Semester like `2009A` from a String, throwing on failure. */
  def unsafeFromString(s: String): Semester =
    fromString(s).getOrElse(sys.error(s"Invalid semester: $s"))

  /** `Semester` is ordered pairwise by its data members. */
  implicit val SemesterOrder: Order[Semester] =
    Order[Year].contramap[Semester](_.year) |+|
    Order[Half].contramap[Semester](_.half)

}
