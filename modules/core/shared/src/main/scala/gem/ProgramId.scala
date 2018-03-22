// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Order, Show }
import cats.implicits._
import java.time._
import java.time.format.DateTimeFormatter
import gem.enum.{ Site, ProgramType, DailyProgramType }
import gem.imp.TimeInstances._
import gem.math.Index
import gem.optics.Format
import gem.parser.ProgramIdParsers
import gem.syntax.parser._

/**
 * A science program id, which has three constructors: [[gem.ProgramId.Science Science]]` for standard
 * programs; [[gem.ProgramId.Science Science]] for standard daily engineering and calibration
 * programs; and [[gem.ProgramId.Nonstandard Nonstandard]]` for all others.
 * @group Program Model
 */
sealed trait ProgramId extends Product with Serializable {

  /**
   * Format a canonical string for this `ProgramId`. This is for human use, and can be round-
   * tripped through `ProgramId.fromString`.
   */
  def format: String

  /** The `Site` associated with this id, if any. */
  def siteOption: Option[Site]

  /** The `Semester` associated with this id, if any. */
  def semesterOption: Option[Semester]

  /** The `ProgramType` associated with this id, if any. */
  def programTypeOption: Option[ProgramType]
}

object ProgramId {

  /** A standard science program id with a site, semester, program type, and positive index. */
  final case class Science private (
    site:        Site,
    semester:    Semester,
    programType: ProgramType,
    index:       Index
  ) extends ProgramId {

    override def format =
      Science.fromString.reverseGet(this)

    override def siteOption: Option[Site] =
      Some(site)

    override def semesterOption: Option[Semester] =
      Some(semester)

    override def programTypeOption: Option[ProgramType] =
      Some(programType)
  }

  object Science {

    /** `Science` program ids are ordered pairwise by their data members. */
    implicit val ScienceOrder: Order[Science] =
      Order.by(a => (a.site, a.semester, a.programType, a.index))

    /** Parse a string into a Science id, and format in the reverse direction. */
    def fromString: Format[String, Science] =
      Format(ProgramIdParsers.science.parseExact, id =>
        s"${id.site.shortName}-${id.semester.format}-${id.programType.shortName}-${id.index.toShort}"
      )

  }

  /** A standard daily program id with a site, program type, and local date. */
  final case class Daily(
    site:             Site,
    dailyProgramType: DailyProgramType,
    localDate:        LocalDate
  ) extends ProgramId {

    /** The first moment of this observing day, 2pm the day before `localDate`. */
    lazy val start: ZonedDateTime =
      ZonedDateTime.of(localDate.minusDays(1), LocalTime.of(14,0,0), site.timezone)

    /** The last moment of this observing day, just before 2pm on 'localDate'. */
    lazy val end: ZonedDateTime =
      ZonedDateTime.of(localDate, LocalTime.of(14,0,0).minusNanos(1), site.timezone)

    /** Semester inferred from the `localDate`. */
    def semester: Semester =
      Semester.fromLocalDate(localDate)

    /** ProgramType inferred from the `dailyProgramType`. */
    def programType: ProgramType =
      dailyProgramType.toProgramType

    /** True if the given instant falls within the observing day defined by `start` and `end`. */
    def includes(i: Instant): Boolean =
      start.toInstant <= i && i <= end.toInstant

    override def format =
      f"${site.shortName}-${dailyProgramType.shortName}${Daily.ymd.format(localDate)}"

    override def siteOption: Option[Site] =
      Some(site)

    override def semesterOption: Option[Semester] =
      Some(semester)

    override def programTypeOption: Option[ProgramType] =
      Some(programType)
  }
  object Daily {

    // shared by all instances
    private val ymd = DateTimeFormatter.ofPattern("yyyyMMdd")

    /** Daily program id for the zoned date and time of the given Site and Instant. */
    def fromSiteAndInstant(site: Site, instant: Instant, programType: DailyProgramType): Daily = {
      val zdt  = ZonedDateTime.ofInstant(instant, site.timezone)
      val end  = zdt.`with`(LocalTime.of(14, 0, 0, 0))
      val zdtʹ = if (zdt.toInstant < end.toInstant) zdt else zdt.plusDays(1)
      apply(site, programType, zdtʹ.toLocalDate)
    }

    /** Parse a `Daily` program id from a string, if possible. */
    def fromString(s: String): Option[Daily] =
      ProgramIdParsers.daily.parseExact(s)

    /** `Daily` program ids are ordered pairwise by their data members. */
    implicit val DailyOrder: Order[Daily] =
      Order.by(a => (a.site, a.dailyProgramType, a.localDate))

  }

  /**
   * Parser for a non-standard program id of the general form `site-semester-type-tail` where
   * any subset of the structured portion is permitted as long as it appears in the proper order.
   * This is the catch-all type for otherwise unparseable ids, so it is guaranteed that the string
   * representation of a `Nonstandard` via `.format` is *not* parseable in to a standard science or
   * daily program id. This data type has no public constructor and no `.copy` method, as these
   * could violate the above invariant. The only way to get an instance is via `.fromString`.
   */
  sealed abstract case class Nonstandard(
    override val siteOption:        Option[Site],
    override val semesterOption:    Option[Semester],
    override val programTypeOption: Option[ProgramType],
                 tail:              String
  ) extends ProgramId {
    override def format =
      Nonstandard.format(siteOption, semesterOption, programTypeOption, tail)

  }
  object Nonstandard {

    /** Format the components of a `Nonstandard`. */
    def format(
      siteOption:        Option[Site],
      semesterOption:    Option[Semester],
      programTypeOption: Option[ProgramType],
      tail:              String
    ): String =
      List(
        siteOption       .map(_.shortName).toList,
        semesterOption   .map(_.format)   .toList,
        programTypeOption.map(_.shortName).toList,
        List(tail)
      ).flatten.intercalate("-")

    /**
     * Parse a `Nonstandard` program id from a string, if possible. Note that this will fail if
     * the the `s` represents a valid science or daily program id.
     */
    def fromString(s: String): Option[Nonstandard] =
      ProgramId.fromString(s) collect {
        case id: Nonstandard => id
      }

    /** `Nonstandard` program ids are ordered pairwise by their data members. */
    implicit val NonStandardOrder: Order[Nonstandard] =
      Order.by(a => (a.siteOption, a.semesterOption, a.programTypeOption, a.tail))

  }

  /** Parse a `ProgramId` from string, if possible. */
  def fromString(s: String): Option[ProgramId] =
    Science.fromString.getOption(s) orElse
    Daily  .fromString(s) orElse
    // Do this only in the last case, and only here, to guarantee you can never get a Nonstandard
    // that can be formatted and re-parsed as a Science or Daily program id. This is important.
    ProgramIdParsers.nonstandard.parseExact(s).map {
      case (os, om, op, t) => new Nonstandard(os, om, op, t) {}
    }

  /** Parse a `ProgramId` from string, throwing on failure. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromString(s: String): ProgramId =
    fromString(s).getOrElse(throw new IllegalArgumentException(s"Invalid program id: $s"))

  /**
   * Programs are ordered lexically by prodict prefix (Daily, Nonstandard, then Science) and then
   * by the defined orderings for individual cases when constructors match.
   */
  implicit val ProgramIdOrder: Order[ProgramId] =
    Order.from {
      case (a: Science,     b: Science)     => a compare b
      case (a: Daily,       b: Daily)       => a compare b
      case (a: Nonstandard, b: Nonstandard) => a compare b
      case (a,              b)              => a.productPrefix compare b.productPrefix
    }

  /**
   * `Ordering` instance for Scala standard library.
   * @see ProgramIdOrder
   */
  implicit val ProgramIdOrdering: scala.math.Ordering[ProgramId] =
    ProgramIdOrder.toOrdering

  implicit val ProgramIdShow: Show[ProgramId] =
    Show.fromToString

}
