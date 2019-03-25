// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.enum.Site

import cats._
import cats.effect.Sync
import cats.implicits._
import java.time._
import monocle.Lens
import monocle.macros.GenLens

/** An observing night is defined as the period of time from 14:00 on one day
  * until 14:00 on the following day.  In a timezone that honors daylight saving,
  * it is sometimes longer and sometimes shorter than a period of 24 hours.
  *
  * An `ObservingNight` pairs a `Site` with a `LocalObservingNight` to obtain
  * precise start/end `Instant`s.
  */
final case class ObservingNight(site: Site, toLocalObservingNight: LocalObservingNight) extends Night {

  /** The `Instant` at which the observing night starts (inclusive) for the
    * associated site.
    */
  override val start: Instant =
    toLocalObservingNight.start.atZone(site.timezone).toInstant

  /** The `Instant` at which the observing night ends (exclusive) for the
    * associated site.
    */
  override val end: Instant =
    toLocalObservingNight.end.atZone(site.timezone).toInstant

  /** The previous observing night. */
  def previous: ObservingNight =
    ObservingNight(site, toLocalObservingNight.previous)

  /** The next observing night. */
  def next: ObservingNight =
    ObservingNight(site, toLocalObservingNight.next)

  /** Returns the local date on which this observing night ends. */
  def toLocalDate: LocalDate =
    toLocalObservingNight.toLocalDate

}

object ObservingNight extends ObservingNightOptics {

  /** Constructs the observing night that ends on the given local date for
    * for the given site.
    *
    * @group Constructors
    */
  def fromSiteAndLocalDate(s: Site, d: LocalDate): ObservingNight =
    ObservingNight(s, LocalObservingNight(d))

  /** Constructs the observing night for the given site and local date time.
    *
    * @group Constructors
    */
  def fromSiteAndLocalDateTime(s: Site, d: LocalDateTime): ObservingNight =
    ObservingNight(s, LocalObservingNight.fromLocalDateTime(d))

  /** Constructs the observing night that includes the given time at the
    * specified site.
    *
    * @group Constructors
    */
  def fromSiteAndInstant(s: Site, i: Instant): ObservingNight =
    ObservingNight(s, LocalObservingNight.fromSiteAndInstant(s, i))

  /** Returns a program in M that computes the ObservingNight for the instant it
    * is executed.
    *
    * @group Constructors
    */
  def current[M[_]: Sync](s: Site): M[ObservingNight] =
    LocalObservingNight.current.map(_.atSite(s))

  /** @group Typeclass Instances. */
  implicit val ShowObservingNight: Show[ObservingNight] =
    Show.fromToString

  /** ObservingNight is ordered by site and local observing night.
    *
    * @group Typeclass Instances
    */
  implicit val OrderObservingNight: Order[ObservingNight] =
    Order.by(n => (n.site, n.toLocalObservingNight))

}

trait ObservingNightOptics {

  /** @group Optics */
  val site: Lens[ObservingNight, Site] =
    GenLens[ObservingNight](_.site)

  /** @group Optics */
  val localObservingNight: Lens[ObservingNight, LocalObservingNight] =
    GenLens[ObservingNight](_.toLocalObservingNight)

  /** @group Optics */
  val localDate: Lens[ObservingNight, LocalDate] =
    localObservingNight composeIso LocalObservingNight.localDate

}