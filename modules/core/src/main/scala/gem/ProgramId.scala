// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import java.time._
import gem.enum.{ Site, ProgramType }
import scalaz._, scalaz.Scalaz.{ char => _, _ }

sealed abstract class ProgramId(
  val siteOption:        Option[Site],
  val semesterOption:    Option[Semester],
  val programTypeOption: Option[ProgramType]
) extends Product with Serializable {

  /**
   * Format a canonical string for this `ProgramId`. This is for human use, and can be round-
   * tripped through `ProgramId.fromString`.
   */
  def format: String

}

object ProgramId {

  sealed abstract case class Science private (
    site:        Site,
    semester:    Semester,
    programType: ProgramType,
    index:       Int
  ) extends ProgramId(
    Some(site),
    Some(semester),
    Some(programType)
  ) {

    // A copy method isn't generated because this class is abstract, so we do so here. It can't
    // support `index` because this could break the invariant that it must be positive.
    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def copy(
      site:        Site        = this.site,
      semester:    Semester    = this.semester,
      programType: ProgramType = this.programType
    ): Science =
      Science.unsafeApply(site, semester, programType, index)

    def format: String =
      s"${site.shortName}-${semester.format}-${programType.shortName}-$index"

  }
  object Science {

    /** Construct a `Science` program id, if `index` is positive. */
    def apply(site: Site, semester: Semester, programType: ProgramType, index: Int): Option[Science] =
      if (index > 0) Some(new Science(site, semester, programType, index) {})
      else None

    /** Construct a `Science` program id, throwing if `index` is positive. */
    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def unsafeApply(site: Site, semester: Semester, programType: ProgramType, index: Int): Science =
      apply(site, semester, programType, index)
        .getOrElse(throw new IllegalArgumentException(s"Program index must be positive: $index"))

    /** Parse a `Science` program id from a string, if possible. */
    def fromString(s: String): Option[ProgramId] =
      Parsers.parseExact(Parsers.programId.science)(s)

    /** `Science` program ids are ordered pairwise by their data members. */
    implicit val ScienceOrder: Order[Science] =
      Order[Site]       .contramap[Science](_.site)        |+|
      Order[Semester]   .contramap[Science](_.semester)    |+|
      Order[ProgramType].contramap[Science](_.programType) |+|
      Order[Int]        .contramap[Science](_.index)

  }

  final case class Daily(
    site:        Site,
    programType: ProgramType,
    localDate:   LocalDate
  ) extends ProgramId(
    Some(site),
    None,
    Some(programType)
  ) {

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

    /** Parse a `Daily` program id from a string, if possible. */
    def fromString(s: String): Option[Daily] =
      Parsers.parseExact(Parsers.programId.daily)(s)

    /** `Daily` program ids are ordered pairwise by their data members. */
    implicit val DailyOrder: Order[Daily] =
      Order[Site]       .contramap[Daily](_.site)        |+|
      Order[ProgramType].contramap[Daily](_.programType) |+|
      Order[LocalDate]  .contramap[Daily](_.localDate)

  }

  final case class Nonstandard(
    override val siteOption:        Option[Site],
    override val semesterOption:    Option[Semester],
    override val programTypeOption: Option[ProgramType],
                 tail:              String
  ) extends ProgramId(
    siteOption,
    semesterOption,
    programTypeOption
  ) {
    def format: String =
      List(
        siteOption       .map(_.shortName).toList,
        semesterOption   .map(_.format)   .toList,
        programTypeOption.map(_.shortName).toList,
        List(tail)
      ).flatten.intercalate("-")
  }
  object Nonstandard {

    /** Parse a `Nonstandard` program id from a string, if possible. */
    def fromString(s: String): Option[Nonstandard] =
      Parsers.parseExact(Parsers.programId.nonstandard)(s)

    /** `Nonstandard` program ids are ordered pairwise by their data members. */
    implicit val NonStandardOrder: Order[Nonstandard] =
      Order[Option[Site]]       .contramap[Nonstandard](_.siteOption)        |+|
      Order[Option[Semester]]   .contramap[Nonstandard](_.semesterOption)    |+|
      Order[Option[ProgramType]].contramap[Nonstandard](_.programTypeOption) |+|
      Order[String]             .contramap[Nonstandard](_.tail)

  }

  /** Parse a `ProgramId` from string, if possible. */
  def fromString(s: String): Option[ProgramId] =
    Parsers.parseExact(Parsers.programId.any)(s)

  /**
   * Programs are ordered lexically by prodict prefix (Daily, Nonstandard, then Science) and then
   * by the defined orderings for individual cases when constructors match.
   */
  implicit val ProgramIdOrder: Order[ProgramId] =
    Order.order {
      case (a: Science,     b: Science)     => a cmp b
      case (a: Daily,       b: Daily)       => a cmp b
      case (a: Nonstandard, b: Nonstandard) => a cmp b
      case (a,              b)              => a.productPrefix cmp b.productPrefix
    }

}
