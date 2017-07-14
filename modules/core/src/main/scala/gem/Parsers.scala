// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import atto._, Atto._, atto.compat.scalaz._
import gem.enum.{ Half, Site, ProgramType, DailyProgramType }
import java.time.{ DateTimeException, Year, Month, LocalDate }
import scalaz._, Scalaz.{ char => _, _ }

/**
 * Module of parsers for various data types used in the science model, for internal use (user code
 * should rely on `fromString` methods on individual data type companions).
 * @group Utilities
 */
object Parsers {

  /** Convenience method for client code. Parse entire input into an Option. */
  def parseExact[A](p: Parser[A])(s: String): Option[A] =
    (p <~ endOfInput).parseOnly(s).option

  /** Convenience method for client code. Parse into an Option, discarding unused input. */
  def parse[A](p: Parser[A])(s: String): Option[A] =
    p.parseOnly(s).option

  /** Parser for a hyphen. */
  val hyphen: Parser[Unit] =
    char('-').map(_ => ())

  /** Parser for a non-whitespace string. */
  val nonWhitespace: Parser[String] =
    stringOf(elem(c => !c.isWhitespace))

  /** Catch a `NumberFormatException`, useful for flatMap. */
  def catchNFE[A, B](f: A => B): A => Parser[B] = a =>
    try ok(f(a)) catch { case e: NumberFormatException => err[B](e.toString) }

  /** Catch a `DateTimeException`, useful for flatMap. */
  def catchDTE[A, B](f: A => B): A => Parser[B] = a =>
    try ok(f(a)) catch { case e: DateTimeException => err[B](e.toString) }

  /** Parser for `n` consecutive digits, parsed as an `Int`. */
  def intN(n:Int): Parser[Int] =
    count(n, digit).map(_.mkString).flatMap(catchNFE(_.toInt)) namedOpaque s"$n-digit int"

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

  /** Parser for a positive `Int`. */
  val positiveInt: Parser[Int] =
    int.filter(_ > 0) namedOpaque "Positive Int"

  /** Parser for an `Enumerated` type, based on some string property. */
  def enumParser[A](f: A => String)(implicit ev: Enumerated[A]): Parser[A] =
    choice(ev.all.map(a => string(f(a)).map(_ => a)))

  /** Parser for `Site` based on `shortName` like `GS`. */
  val site: Parser[Site] =
    enumParser[Site](_.shortName) namedOpaque "Site"

  /** Parser for `ProgramType` based on `shortName` like `SV`. */
  val programType: Parser[ProgramType] =
    enumParser[ProgramType](_.shortName) namedOpaque "ProgramType"

  /** Parser for `DailyProgramType` based on `shortName` like `CAL`. */
  val dailyProgramType: Parser[DailyProgramType] =
    enumParser[DailyProgramType](_.shortName) namedOpaque "DailyProgramType"

  /** Parser for `Half` based on `tag` like `A`. */
  val half: Parser[Half] =
    enumParser[Half](_.tag) namedOpaque "Half"

  /** Parser for a full-year `Semester` like `2015A`. */
  val semester: Parser[Semester] =
    (year4 |@| half)(Semester.apply)

  /** Module of ProgramId parsers. */
  object programId {
    import ProgramId._

    /** Parser for a standard science program id like `GS-2008A-SV-33`. */
    val science: Parser[Science] =
      for {
        s <- site        <~ hyphen
        m <- semester    <~ hyphen
        t <- programType <~ hyphen
        n <- positiveInt
      } yield Science.unsafeApply(s, m, t, n) // we know n is positive

    /** Parser for a daily program id like `GS-ENG20120102`. */
    val daily: Parser[Daily] =
      for {
        s <- site <~ hyphen
        t <- dailyProgramType
        d <- yyyymmdd
      } yield Daily(s, t, d)

    /**
     * Parser for the components of a nonstandard program id (which has no public constructor).
     * This parser is greedy and will read as much structured information as possible rather than
     * leaving it in the tail.
     */
    def nonstandard: Parser[(Option[Site], Option[Semester], Option[ProgramType], String)] =
      for {
        os <- opt(site        <~ hyphen)
        om <- opt(semester    <~ hyphen)
        op <- opt(programType <~ hyphen)
        t  <- nonWhitespace
      } yield (os, om, op, t)

  }

}
