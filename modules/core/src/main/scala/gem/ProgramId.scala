// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import java.time._
import java.time.format.DateTimeFormatter
import gem.enum.{ Site, ProgramType, DailyProgramType }
import gem.imp.TimeInstances._
import scalaz._, scalaz.Scalaz.{ char => _, _ }

/**
 * A science program id, which has three constructors: [[gem.ProgramId.Science Science]]` for standard
 * programs; [[gem.ProgramId.Science Science]] for standard daily engineering and calibration
 * programs; and [[gem.ProgramId.Nonstandard Nonstandard]]` for all others.
 * @group Program Model
 */
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

  /** A standard science program id with a site, semester, program type, and positive index. */
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

    override def format =
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

  /** A standard daily program id with a site, program type, and local date. */
  final case class Daily(
    site:             Site,
    dailyProgramType: DailyProgramType,
    localDate:        LocalDate
  ) extends ProgramId(
    Some(site),
    None,
    Some(dailyProgramType.toProgramType)
  ) {

    /** The first moment of this observing day, 2pm the day before `localDate`. */
    lazy val start: ZonedDateTime =
      ZonedDateTime.of(localDate, LocalTime.MIDNIGHT, site.timezone).minusHours(10)

    /** The last moment of this observing day, just before 2pm on 'localDate'. */
    lazy val end: ZonedDateTime =
      ZonedDateTime.of(localDate.minusDays(1), LocalTime.MAX, site.timezone).plusHours(14)

    /** True if the given instant falls within the observing day defined by `start` and `end`. */
    def includes(i: Instant): Boolean =
      start.toInstant <= i && i <= end.toInstant

    override def format =
      f"${site.shortName}-${dailyProgramType.shortName}${Daily.ymd.format(localDate)}"

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
      Parsers.parseExact(Parsers.programId.daily)(s)

    /** `Daily` program ids are ordered pairwise by their data members. */
    implicit val DailyOrder: Order[Daily] =
      Order[Site]            .contramap[Daily](_.site)                           |+|
      Order[DailyProgramType].contramap[Daily](_.dailyProgramType) |+|
      Order[LocalDate]       .contramap[Daily](_.localDate)

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
  ) extends ProgramId(
    siteOption,
    semesterOption,
    programTypeOption
  ) {
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
      Order[Option[Site]]       .contramap[Nonstandard](_.siteOption)        |+|
      Order[Option[Semester]]   .contramap[Nonstandard](_.semesterOption)    |+|
      Order[Option[ProgramType]].contramap[Nonstandard](_.programTypeOption) |+|
      Order[String]             .contramap[Nonstandard](_.tail)

  }

  /** Parse a `ProgramId` from string, if possible. */
  def fromString(s: String): Option[ProgramId] =
    Science.fromString(s) orElse
    Daily  .fromString(s) orElse
    // Do this only in the last case, and only here, to guarantee you can never get a Nonstandard
    // that can be formatted and re-parsed as a Science or Daily program id. This is important.
    Parsers.parseExact(Parsers.programId.nonstandard)(s).map {
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
    Order.order {
      case (a: Science,     b: Science)     => a cmp b
      case (a: Daily,       b: Daily)       => a cmp b
      case (a: Nonstandard, b: Nonstandard) => a cmp b
      case (a,              b)              => a.productPrefix cmp b.productPrefix
    }

  /**
   * `Ordering` instance for Scala standard library.
   * @see ProgramIdOrder
   */
  implicit val ProgramIdOrdering: scala.math.Ordering[ProgramId] =
    ProgramIdOrder.toScalaOrdering

  implicit val ProgramIdShow: Show[ProgramId] =
    Show.showA

}
