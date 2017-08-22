// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.implicits._
import atto._, Atto._
import gem.enum.{ Half, Site, ProgramType, DailyProgramType }
import gem.math._
import java.time.{ DateTimeException, Year, Month, LocalDate }


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
    char('-').void

  /** Parser for one or more spaces. */
  val spaces1: Parser[Unit] =
    skipMany1(char(' '))

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
    (year4, half).mapN(Semester.apply)

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

  /** Parser for an `Epoch.Scheme`. */
  val epochScheme: Parser[Epoch.Scheme] =
    char('B').as(Epoch.Besselian) |
    char('J').as(Epoch.Julian)

  /** Parser for an `Epoch`. */
  val epoch: Parser[Epoch] =
    for {
      scheme <- epochScheme
      year   <- int
      _      <- char('.')
      frac   <- intN(3)
    } yield scheme.fromMilliyears(year * 1000 + frac)

  /** Module of parsers for angles. */
  object angle {

    // Integral exponentiation
    implicit class MoreIntOps(n: Int) {
      def **(e: Int): Int =
        if (e < 1) 1 else n * (** (e - 1))
    }

    // An optional signum, here represented as a boolean indicating whether to negate.
    val neg: Parser[Boolean] =
      opt(char('-').void || char('+').void).map {
        case Some(Left(()))  => true
        case Some(Right(())) => false
        case None            => false
      }

    /**
     * Fractional portion of a decimal value, with up to N places given. So frac(3) parsing "12"
     * yields 120. Mind the overflow, this only works for small N.
     */
    def frac(n: Int): Parser[Int] =
      if (n < 1) ok(0)
      else opt(digit.map(_ - '0')) flatMap {
        case None    => ok(0)
        case Some(d) => frac(n - 1).map(_ + d * (10 ** (n - 1)))
      }

    /**
     * Generic parser for the components of an angle in "11 22 33.444555" format, with at least 1
     * and at most 6 digits following the decimal point, and terminal parsers for each segment.
     */
    def genAngle(t1: Parser[_], t2: Parser[_], t3: Parser[_]): Parser[(Int, Int, Int, Int, Int)] =
      for {
        h  <- int <~ t1
        m  <- int <~ t2
        s  <- int <~ char('.')
        µs <- frac(6)
        _  <- t3
      } yield (h, m, s, µs / 1000, µs % 1000)

    /** Generic parser for the components of an HourAngle; see `genAngle`. */
    def genHMS(t1: Parser[_], t2: Parser[_], t3: Parser[_]): Parser[HourAngle] =
      genAngle(t1, t2, t3).map((HourAngle.fromHMS _).tupled)

    /** 00:00:00.000000 */
    val hms1: Parser[HourAngle] =
      genHMS(char(':'), char(':'), ok(()))

    /** 00 00 00.000000 */
    val hms2: Parser[HourAngle] =
      genHMS(spaces1, spaces1, ok(()))

    /** 00h 00m 00.000000s */
    val hms3: Parser[HourAngle] =
      genHMS(token(char('h')), token(char('m')), char('s'))

    val hms = hms1 | hms2 | hms3

    /** Generic parser for the components of an HourAngle; see `genAngle`. */
    private def genDMS(t1: Parser[_], t2: Parser[_], t3: Parser[_]): Parser[Angle] =
      for {
        n <- neg
        a <- genAngle(t1, t2, t3).map((Angle.fromDMS _).tupled)
      } yield {
        if (n) -a else a
      }

    /** +00:00:00.000000 */
    val dms1: Parser[Angle] =
      genDMS(char(':'), char(':'), ok(()))

    /** +00 00 00.000000 */
    val dms2: Parser[Angle] =
      genDMS(spaces1, spaces1, ok(()))

    /** +04° 41′ 36.2072″ */
    val dms3: Parser[Angle] =
      genDMS(token(char('°')), token(char('′')), token(char('″')))

    val dms = dms1 | dms2 | dms3

  }

  /** Parser for a RightAscension, always a positive angle in HMS. */
  val ra: Parser[RightAscension] =
    angle.hms.map(RightAscension(_))

  /** Parser for a RightAscension, always a positive angle in HMS. */
  val dec: Parser[Declination] =
    angle.dms.map(Declination.fromAngle).flatMap {
      case Some(ra) => ok(ra)
      case None     => err("Invalid Declination")
    }

  /** Parser for coordinates: HMS and DMS separated by spaces. */
  val coordinates: Parser[Coordinates] =
    (token(ra), dec).mapN(Coordinates(_, _))

}
