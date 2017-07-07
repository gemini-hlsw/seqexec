// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import java.time._
import gem.enum.{ Site, ProgramType }
import scalaz.Scalaz.{ char => _, _ }

sealed abstract class ProgramId(
  val siteOption:        Option[Site],
  val semesterOption:    Option[Semester],
  val programTypeOption: Option[ProgramType]
) {

  /**
   * Format a canonical string for this `ProgramId`. This is for human use, and can be round-
   * tripped through `ProgramId.fromString`.
   */
  def format: String

}

object ProgramId {

  final case class Science(site: Site, semester: Semester, programType: ProgramType, index: Int)
    extends ProgramId(Some(site), Some(semester), Some(programType)) {
    require(index >= 0) // no me gusta
    def format: String =
      s"${site.shortName}-${semester.toString}-${programType.shortName}-$index"
  }
  object Science {
    def fromString(s: String): Option[ProgramId] =
      Parsers.parseExact(Parsers.programId.science)(s)
  }

  final case class Daily(site: Site, programType: ProgramType, localDate: LocalDate)
    extends ProgramId(Some(site), None, Some(programType)) {

    lazy val start: ZonedDateTime =
      ZonedDateTime.of(localDate, LocalTime.MIDNIGHT, site.timezone).minusHours(10) // 2pm the day before

    lazy val end: ZonedDateTime =
      start.plusDays(1)

    def includes(i: Instant): Boolean =
      start.toInstant <= i && i < end.toInstant

    def format: String = {
      val (yyyy, mm, dd) = (localDate.getYear, localDate.getMonth.getValue, localDate.getDayOfMonth)
      f"${site.shortName}-${programType.shortName}$yyyy%04d$mm%02d$dd%02d"
    }

  }
  object Daily {
    def fromString(s: String): Option[Daily] =
      Parsers.parseExact(Parsers.programId.daily)(s)
  }

  // Nonstandard program types
  final case class Nonstandard(
    override val siteOption:        Option[Site],
    override val semesterOption:    Option[Semester],
    override val programTypeOption: Option[ProgramType],
                 tail:              String
  ) extends ProgramId(siteOption, semesterOption, programTypeOption) {
    def format: String =
      List(
        siteOption       .map(_.shortName).toList,
        semesterOption   .map(_.format)   .toList,
        programTypeOption.map(_.shortName).toList,
        List(tail)
      ).flatten.intercalate("-")
  }
  object Nonstandard {
    def fromString(s: String): Option[Nonstandard] =
      Parsers.parseExact(Parsers.programId.nonstandard)(s)
  }

  def fromString(s: String): Option[ProgramId] =
    Parsers.parseExact(Parsers.programId.any)(s)

}
