// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Order, Show }
import cats.effect.IO
import cats.implicits._
import gem.enum.{ Half, Site }
import gem.instances.time._
import gem.math.LocalObservingNight
import gem.parser.SemesterParsers
import gsp.math.syntax.parser._
import java.time._
import java.time.Month._

/**
 * A (Year, Half) pair.
 * @group Program Model
 */
final case class Semester(year: Year, half: Half) {

  /**
   * This Semester plus the given number of years. Raises a `DateTimeException` for final year out
   * of range -999999999 - 999999999.
   */
  def plusYears(n: Int): Semester =
    copy(year = year.plusYears(n.toLong))

  /** This Semester plus the given number of half-years. */
  def plusSemesters(n: Int): Semester = {
    val yy = year.getValue
    val hs = yy * 2 + half.toInt * yy.signum + n
    Semester(Year.of(hs / 2), Half.unsafeFromInt(hs.abs % 2))
  }

  /** The semester immediately following this one. */
  def next: Semester =
    plusSemesters(1)

  /** The semester immediately preceding this one. */
  def prev: Semester =
    plusSemesters(-1)

  /** Module of various representations of the first instant of the semester, *inclusive*. */
  object start {
    lazy val yearMonth: YearMonth = YearMonth.of(year.getValue, half.startMonth)
    lazy val localDate: LocalDate = LocalDate.of(year.getValue, half.startMonth, 1)
    lazy val localDateTime: LocalDateTime = LocalDateTime.of(localDate, LocalTime.MIDNIGHT).minusHours(10) // 2pm the day before
    def zonedDateTime(zoneId: ZoneId): ZonedDateTime = ZonedDateTime.of(localDateTime, zoneId)
    def atSite(site: Site): ZonedDateTime = zonedDateTime(site.timezone)
  }

  /**
   * Module of various representations of the last instant of the semester, *inclusive*.
   * @group Program Model
   */
  object end {
    private def nextYear = (half match { case Half.A => year; case Half.B => year.plusYears(1) }).getValue
    lazy val yearMonth: YearMonth = YearMonth.of(nextYear, half.endMonth)
    lazy val localDate: LocalDate = LocalDate.of(nextYear, half.endMonth, half.endMonth.maxLength)
    lazy val localDateTime: LocalDateTime = LocalDateTime.of(localDate.minusDays(1), LocalTime.MAX).plusHours(14) // 2pm today
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

  /**
   * Semester for the specified year and month, exclusive of semester start and inclusive of
   * semester end. The semester actually starts at 2pm on the 31st of the January/August, but
   * this method gives the entire switchover day to the previous semester. A simpler way of saying
   * this is that this method ignores the day of the month. If you need more precision you must
   * use a conversion method that includes the time of day.
   */
  def fromLocalDate(d: LocalDate): Semester =
    fromYearMonth(YearMonth.of(d.getYear, d.getMonth))

  /**
   * Semester for the specified local date and time. This handles the 2pm switchover on the last
   * day of the semester.
   */
  def fromLocalDateTime(d: LocalDateTime): Semester = {
    val dʹ = LocalObservingNight.fromLocalDateTime(d).toLocalDate
    fromYearMonth(YearMonth.of(dʹ.getYear, dʹ.getMonth))
  }

  /**
   * Semester for the specified zoned date and time. This handles the 2pm switchover on the last
   * day of the semester.
   */
  def fromZonedDateTime(d: ZonedDateTime): Semester =
    fromLocalDateTime(d.toLocalDateTime)

  /** Semester for the zoned date and time of the given Site and Instant. */
  def fromSiteAndInstant(s: Site, i: Instant): Semester =
    fromZonedDateTime(ZonedDateTime.ofInstant(i, s.timezone))

  /** Current semester. */
  def current(s: Site): IO[Semester] =
    IO(LocalDateTime.now(s.timezone)).map(fromLocalDateTime)

  /** Parse a full-year Semester like `2009A` from a String, if possible. */
  def fromString(s: String): Option[Semester] =
    SemesterParsers.semester.parseExact(s)

  /** Parse a full-year Semester like `2009A` from a String, throwing on failure. */
  def unsafeFromString(s: String): Semester =
    fromString(s).getOrElse(sys.error(s"Invalid semester: $s"))

  /** `Semester` is ordered pairwise by its data members. */
  implicit val SemesterOrder: Order[Semester] =
    Order.by(a => (a.year, a.half))

  /**
   * `Ordering` instance for Scala standard library.
   * @see SemesterOrder
   */
  implicit val SemesterOrdering: scala.math.Ordering[Semester] =
    SemesterOrder.toOrdering

  implicit val SemesterShow: Show[Semester] =
    Show.fromToString

}
