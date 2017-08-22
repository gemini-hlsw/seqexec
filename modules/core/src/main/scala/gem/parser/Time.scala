// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.parser

import atto._, Atto._
import java.time.{ DateTimeException, Year, Month, LocalDate }

/** Parsers for `java.time` data types. */
trait TimeParsers {
  import MiscParsers.intN

  /** Catch a `DateTimeException`, useful for flatMap. */
  def catchDTE[A, B](f: A => B): A => Parser[B] = a =>
    try ok(f(a)) catch { case e: DateTimeException => err[B](e.toString) }

  /** Parser for 4 consecutive digits, parsed as a `Year`. */
  val year4: Parser[Year] =
    intN(4).flatMap(catchDTE(Year.of)) namedOpaque "Year"

  /** Parser for 2 consecutive digits, parsed as a `Month`. */
  val month2: Parser[Month] =
    intN(2).flatMap(catchDTE(Month.of)) namedOpaque "Month"

  /** Parser for a `LocalDate` in the form `20151107`. */
  def yyyymmdd: Parser[LocalDate] =
    for {
      y <- year4
      m <- month2
      d <- intN(2) namedOpaque "2-digit day of month"
      a <- catchDTE((d: Int) => LocalDate.of(y.getValue, m, d))(d)
    } yield a

}
object TimeParsers extends TimeParsers
