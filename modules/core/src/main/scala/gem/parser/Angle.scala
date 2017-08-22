// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package parser

import atto._, Atto._
import gem.math.{ Angle, HourAngle }

/** Parsers for `[[gem.math.Angle]].` */
trait AngleParsers {
  import MiscParsers._

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

  /** Generic parser for the components of a DMS Angle; see `genAngle`. */
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

object AngleParsers extends AngleParsers
