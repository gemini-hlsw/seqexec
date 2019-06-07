// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package parser

import atto._, Atto._
import cats.implicits._
import gem.enum.{ Site, ProgramType }
import gsp.math.parser.{ MiscParsers, TimeParsers }

/** Parsers for [[gem.ProgramId]]. */
trait ProgramIdParsers {
  import EnumParsers._
  import SemesterParsers._
  import ProgramId._
  import MiscParsers._
  import TimeParsers._

  /** Parser for a standard science program id like `GS-2008A-SV-33`. */
  val science: Parser[Science] =
    (site        <~ hyphen,
     semester    <~ hyphen,
     programType <~ hyphen,
     index).mapN(Science.apply) named "science"

  /** Parser for a daily program id like `GS-ENG20120102`. */
  val daily: Parser[Daily] =
    (site <~ hyphen, dailyProgramType, yyyymmdd).mapN(Daily.apply) named "daily"

  /**
   * Parser for the components of a nonstandard program id (which has no public constructor).
   * This parser is greedy and will read as much structured information as possible rather than
   * leaving it in the tail.
   */
  def nonstandard: Parser[(Option[Site], Option[Semester], Option[ProgramType], String)] =
    (opt(site        <~ hyphen),
     opt(semester    <~ hyphen),
     opt(programType <~ hyphen),
     nonWhitespace).tupled named "nonstandard"

}
object ProgramIdParsers extends ProgramIdParsers
