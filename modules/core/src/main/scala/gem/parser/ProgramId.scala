// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package parser

import atto._, Atto._
import gem.enum.{ Site, ProgramType }

/** Parsers for [[gem.ProgramId]]. */
trait ProgramIdParsers {
  import MiscParsers._
  import EnumParsers._
  import SemesterParsers._
  import TimeParsers._
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
object ProgramIdParsers extends ProgramIdParsers
