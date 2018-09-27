// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.parser

import cats.implicits._

import atto._, Atto._
import java.time.{ DateTimeException, Instant, LocalDate, LocalDateTime, LocalTime, Month, Year, ZoneOffset }

/** Parsers for `java.time` data types. */
trait TimeParsers {

  import MiscParsers.{ dot, frac, intN, spaces1, void }

  /** Catch a `DateTimeException`, useful for flatMap. */
  def catchDTE[A, B](f: A => B): A => Parser[B] = a =>
    try ok(f(a)) catch { case e: DateTimeException => err[B](e.toString) }

  /** Parser for 4 consecutive digits, parsed as a `Year`. */
  val year4: Parser[Year] =
    intN(4).flatMap(catchDTE(Year.of)) namedOpaque "year4"

  /** Parser for 2 consecutive digits, parsed as a `Month`. */
  val month2: Parser[Month] =
    intN(2).flatMap(catchDTE(Month.of)) namedOpaque "month2"

  /** Parser for 3 letter month strings like "Jan", parsed as a `Month`. */
  val monthMMM: Parser[Month] = {
    import Month._

    val months = List(
      "Jan" -> JANUARY, "Feb" -> FEBRUARY, "Mar" -> MARCH,
      "Apr" -> APRIL,   "May" -> MAY,      "Jun" -> JUNE,
      "Jul" -> JULY,    "Aug" -> AUGUST,   "Sep" -> SEPTEMBER,
      "Oct" -> OCTOBER, "Nov" -> NOVEMBER, "Dec" -> DECEMBER
    )

    choice(months.map { case (s, m) => string(s).as(m) }) namedOpaque "monthMMM"
  }

  /** Generic parser for a local date string in the order year, month, day.
    *
    * @param month parser for the month component
    * @param sep   parser for any separator between components
    */
  def genYMD(month: Parser[Month], sep: Parser[_]): Parser[LocalDate] =
    (for {
      y <- year4 <~ sep
      m <- month <~ sep
      d <- intN(2) namedOpaque "2-digit day of month"
      l <- catchDTE((d: Int) => LocalDate.of(y.getValue, m, d))(d)
    } yield l) named s"genYMD($month, $sep)"

  /** Parser for a `LocalDate` in the form `20151107`. */
  def yyyymmdd: Parser[LocalDate] =
    genYMD(month2, void) named "yyyymmdd"

  /** Generic parser for a `LocalTime` with fractional seconds to nanosecond
    * precision.
    *
    * @param sep parser for any separator between hours, minutes, and seconds
    */
  def genLocalTime(sep: Parser[_]): Parser[LocalTime] = {
    val nano: Parser[Int] =
      opt(dot ~> frac(9)).flatMap(o => ok(o.getOrElse(0))) namedOpaque "up to 9 digits nanoseconds"

    (for {
      h  <- (intN(2) namedOpaque "2-digit hour of day")     <~ sep
      m  <- (intN(2) namedOpaque "2-digit minute of hour")  <~ sep
      s  <- (intN(2) namedOpaque "2-digit second of minute")
      ns <- nano
      t  <- catchDTE((_: Int) => LocalTime.of(h, m, s, ns))(ns)
    } yield t) named "00:00:00.000000000"
  }

  /** Parser for instants in UTC, where the date is followed by the time and
    * separated by spaces.
    */
  def instantUTC(date: Parser[LocalDate], time: Parser[LocalTime]): Parser[Instant] =
    (date <~ spaces1, time).mapN { case (d, t) =>
      LocalDateTime.of(d, t).toInstant(ZoneOffset.UTC)
    }
}
object TimeParsers extends TimeParsers
