// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model
package parser

import atto._
import cats.syntax.all._
import lucuma.core.enum.ProgramType
import lucuma.core.enum.Site
import lucuma.core.model.Semester
import lucuma.core.parser.MiscParsers
import lucuma.core.parser.TimeParsers
import seqexec.model.enum.DailyProgramType

import Atto._

/** Parsers for ProgramId. */
trait ProgramIdParsers {
  import lucuma.core.enum.parser.EnumParsers._
  import lucuma.core.model.parser.SemesterParsers._
  import ProgramId._
  import MiscParsers._
  import TimeParsers._

  /** Parser for `DailyProgramType` based on `shortName` like `CAL`. */
  val dailyProgramType: Parser[DailyProgramType] =
    enumBy[DailyProgramType](_.shortName).namedOpaque("DailyProgramType")

  /** Parser for a standard science program id like `GS-2008A-SV-33`. */
  val science: Parser[Science] =
    (site <~ hyphen, semester <~ hyphen, programType <~ hyphen, index)
      .mapN(Science.apply)
      .named("science")

  /** Parser for a daily program id like `GS-ENG20120102`. */
  val daily: Parser[Daily] =
    (site <~ hyphen, dailyProgramType, yyyymmdd).mapN(Daily.apply).named("daily")

  /**
   * Parser for the components of a nonstandard program id (which has no public constructor).
   * This parser is greedy and will read as much structured information as possible rather than
   * leaving it in the tail.
   */
  def nonstandard: Parser[(Option[Site], Option[Semester], Option[ProgramType], String)] =
    (opt(site <~ hyphen), opt(semester <~ hyphen), opt(programType <~ hyphen), nonWhitespace).tupled
      .named("nonstandard")

}
object ProgramIdParsers extends ProgramIdParsers
