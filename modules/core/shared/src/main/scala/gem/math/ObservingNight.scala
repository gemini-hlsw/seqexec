// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.enum.Site

import cats._
import cats.data.Validated
import cats.effect.IO
import cats.implicits._

import java.time._
import java.time.format.DateTimeFormatter

import scala.math.Ordering.Implicits._


/** An observing night is defined as the period of time from 14:00 on one day
  * until 14:00 on the following day.  In a timezone that honors daylight saving,
  * it is sometimes onger and sometimes shorter than a period of 24 hours.  This
  * is also true of days which contain leap seconds.
  */
sealed abstract case class ObservingNight(start: Instant, end: Instant, site: Site) extends Night {

  // Sanity check ... should be correct via the companion constructor
  assert(start < end)

  def previous: ObservingNight =
    ObservingNight.forInstant(start.minusNanos(1L), site)

  def next: ObservingNight =
    ObservingNight.forInstant(end, site)

  def format: String =
    ObservingNight.Formatter.withZone(site.timezone).format(end)

}

object ObservingNight {

  /** The hour, in the local time zone, at which the night is considered to
    * officially start.
    *
    * @group Constants
    */
  val LocalNightStartHour: Int =
    14

  /** The local time at which the night is considered to officially start.
    *
    * @group Constants
    */
  val LocalNightStart: LocalTime =
    LocalTime.of(LocalNightStartHour, 0)

  /** Formatter for nights.  The night string representation corresponds to the
    * date YYYYMMDD for which the night ends in UTC.
    *
    * @group Constants
    */
  val Formatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyyMMdd")

  /** Returns a IO that when executed the observing night corresponding to the
    * current instant.
    *
    * @group Constructors
    */
  def now(s: Site): IO[ObservingNight] =
    IO(Instant.now).map(forInstant(_, s))

  /** Constructs the observing night that includes the given time at the
    * specified site.
    *
    * @group Constructors
    */
  def forInstant(i: Instant, s: Site): ObservingNight = {
    val zdt   = ZonedDateTime.ofInstant(i, s.timezone)
    val bound = zdt.withHour(LocalNightStartHour)
                   .withMinute(0)
                   .withSecond(0)
                   .withNano(0)

    val (start, end) =
      if (zdt.toLocalTime >= LocalNightStart) (bound, bound.plusDays(1L))
      else (bound.minusDays(1L), bound)

    new ObservingNight(start.toInstant, end.toInstant, s) {}
  }

  /** Constructs the observing night for the given year, month, and day at the
    * given site.
    *
    * @group Constructors
    */
  def forYMD(year: Year, month: Month, day: Int, site: Site): Option[ObservingNight] =
    Validated.catchNonFatal {
      ZonedDateTime.of(year.getValue, month.getValue, day, LocalNightStartHour, 0, 0, 0, site.timezone)
                   .minusNanos(1L)
                   .toInstant
    }.toOption.map(forInstant(_, site))


  /** Parses a night string of the form YYYYMMDD into an `ObservingNight` for
    * the given `Site`.
    */
  def parse(nightString: String, site: Site): Option[ObservingNight] =
    Validated.catchNonFatal { LocalDate.parse(nightString, Formatter) }
             .toOption
             .map { localDate =>
               ObservingNight.forInstant(
                 ZonedDateTime.of(localDate, LocalNightStart, site.timezone)
                              .minusNanos(1L)
                              .toInstant,
                 site)
             }


  /** @group Typeclass Instances. */
  implicit val ShowObservingNight: Show[ObservingNight] =
    Show.fromToString

  /** ObservingNight is ordered by start time.
    *
    * @group Typeclass Instances
    */
  implicit val OrderObservingNight: Order[ObservingNight] =
    Order.by(n => (n.start.getEpochSecond, n.start.getNano))

}
