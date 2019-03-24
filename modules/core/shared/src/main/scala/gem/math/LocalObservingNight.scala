// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats._
import cats.data.Validated
import cats.effect.Sync
import cats.implicits._
import gem.enum.Site
import gem.instances.time._
import java.time._
import java.time.format.DateTimeFormatter
import monocle.Iso
import monocle.macros.GenIso

/** A local observing night is defined as the period of time starting at 14:00
  * (inclusive) on one day and ending at 14:00 (exclusive) the next day.
  *
  * A `LocalObservingNight` is site-agnostic.  Use `atSite` to obtain precise
  * start / end times in an `ObservingNight`.
  */
final case class LocalObservingNight(toLocalDate: LocalDate) {

  /** Constructs an ObservingNight associated with a particular Site. */
  def atSite(site: Site): ObservingNight =
    ObservingNight(site, this)

  /** The start time (inclusive) of the local observing night. */
  def start: LocalDateTime =
    end.minusDays(1L)

  /** The end time (exclusive) of the local observing night. */
  def end: LocalDateTime =
    LocalDateTime.of(toLocalDate, LocalObservingNight.Start)

  /** The previous local observing night. */
  def previous: LocalObservingNight =
    LocalObservingNight(toLocalDate.minusDays(1L))

  /** The next local observing night. */
  def next: LocalObservingNight =
    LocalObservingNight(toLocalDate.plusDays(1L))

  /** Returns `true` if the local date time is included in this night, but
    * `false` otherwise.
    */
  def includes(d: LocalDateTime): Boolean =
    (start <= d) && (d < end)

  /** Formats the LocalObservingNight to a String `YYYYMMDD` that is readable by
    * `LocalObservingNight.fromString`.
    */
  def format: String =
    LocalObservingNight.Formatter.format(toLocalDate)
}

object LocalObservingNight extends LocalObservingNightOptics {

  /** The hour, in the local time zone, at which the night is considered to
    * officially start.
    *
    * @group Constants
    */
  val StartHour: Int =
    14

  /** The local time at which the night is considered to officially start.
    *
    * @group Constants
    */
  val Start: LocalTime =
    LocalTime.of(StartHour, 0)

  /** Formatter for nights.  The night string representation corresponds to the
    * date YYYYMMDD for which the night ends in UTC.
    *
    * @group Constants
    */
  val Formatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyyMMdd")

  /** Constructs the LocalObservingNight corresponding to the given date and
    * time, taking into account the 2PM switchover hour.
    *
    * @group Constructors
    */
  def fromLocalDateTime(d: LocalDateTime): LocalObservingNight =
    LocalObservingNight(
      d.toLocalDate.plusDays(if (d.toLocalTime >= Start) 1L else 0L)
    )

  /** Constructs the LocalObservingNight corresponding to the given time, taking
    * into account the 2PM switchover hour.
    *
    * @group Constructors
    */
  def fromZonedDateTime(d: ZonedDateTime): LocalObservingNight =
    fromLocalDateTime(d.toLocalDateTime)

  /** Constructs the LocalObservingNight corresponding to the given time, taking
    * into account the 2PM switchover hour.
    *
    * @group Constructors
    */
  def fromSiteAndInstant(s: Site, i: Instant): LocalObservingNight =
    fromZonedDateTime(ZonedDateTime.ofInstant(i, s.timezone))

  /** Returns a program in M that computes the LocalObservingNight for the
    * instant it is executed.
    *
    * @group Constructors
    */
  def current[M[_]: Sync]: M[LocalObservingNight] =
    Sync[M].delay(LocalDateTime.now).map(fromLocalDateTime)

  /** Parse a LocalObservingNight like `20180307` from a String, if possible. */
  def fromString(s: String): Option[LocalObservingNight] =
    Validated.catchNonFatal { LocalDate.parse(s, Formatter) }
             .toOption
             .map(LocalObservingNight(_))

  /** Parse a LocalObservingNight like `20180307` from a String, throwing on
    * failure.
    */
  def unsafeFromString(s: String): LocalObservingNight =
    fromString(s).getOrElse(sys.error(s"Invalid night: $s"))

  /** @group Typeclass Instances. */
  implicit val ShowLocalObservingNight: Show[LocalObservingNight] =
    Show.fromToString

  /** LocalObservingNight is ordered by local date.
    *
    * @group Typeclass Instances
    */
  implicit val OrderLocalObservingNight: Order[LocalObservingNight] =
    Order.by(_.toLocalDate)
}

trait LocalObservingNightOptics {

  /** @group Optics */
  val localDate: Iso[LocalObservingNight, LocalDate] =
    GenIso[LocalObservingNight, LocalDate]

}