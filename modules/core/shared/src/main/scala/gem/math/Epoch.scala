// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package math

import cats.{ Order, Show }
import cats.implicits._
import gem.parser.EpochParsers
import gem.syntax.parser._
import gem.optics.Format
import java.time._

/**
 * An epoch, the astronomer's equivalent of `Instant`, based on a fractional year in some temporal
 * scheme (Julian or Besselian) that determines year zero and the length of a year. The only
 * meaningful operation for an `Epoch` is to ask the elapsed epoch-years between it and some other
 * point in time. We need this for proper motion corrections because velocities are measured in
 * motion per epoch-year. The epoch year is stored internally as integral milliyears.
 * @param scheme This `Epoch`'s temporal scheme.
 * @see The Wikipedia [[https://en.wikipedia.org/wiki/Epoch_(astronomy) article]]
 */
final class Epoch private (val scheme: Epoch.Scheme, private[math] val toMilliyears: Int) {

  /** This `Epoch`'s year. Note that this value is not very useful without the `Scheme`. */
  def epochYear: Double =
    toMilliyears.toDouble * 1000.0

  /** Offset in epoch-years from this `Epoch` to the given `Instant`. */
  def untilInstant(i: Instant): Double =
    untilLocalDateTime(LocalDateTime.ofInstant(i, ZoneOffset.UTC))

  /** Offset in epoch-years from this `Epoch` to the given `LocalDateTime`. */
  def untilLocalDateTime(ldt: LocalDateTime): Double =
    untilJulianDay(Epoch.Scheme.toJulianDay(ldt))

  /** Offset in epoch-years from this `Epoch` to the given fractional Julian day. */
  def untilJulianDay(jd: Double): Double =
    untilEpochYear(scheme.fromJulianDay(jd).epochYear)

  /** Offset in epoch-years from this `Epoch` to the given epoch year under the same scheme. */
  def untilEpochYear(epochYear: Double): Double =
    epochYear - this.epochYear

  def plusYears(y: Double): Epoch =
    scheme.fromEpochYears(epochYear + y)

  override def equals(a: Any): Boolean =
    a match {
      case e: Epoch => (scheme === e.scheme) && toMilliyears === e.toMilliyears
      case _ => false
    }

  override def hashCode: Int =
    scheme.hashCode ^ toMilliyears

  override def toString =
    Epoch.fromString.taggedToString("Epoch", this)

}

object Epoch extends EpochOptics {

  /**
   * Standard epoch.
   * @group Constructors
   */
  val J2000: Epoch = Julian.fromIntegralYears(2000)

  /**
   * Standard epoch prior to J2000. Obsolete but still in use.
   * @group Constructors
   */
  val B1950: Epoch = Besselian.fromIntegralYears(1950)

  /**
   * The scheme defines year zero and length of a year in terms of Julian days. There are two
   * common schemes that we support here.
   */
  sealed abstract class Scheme(
    val prefix:       Char,
    val yearBasis:    Double,
    val julianBasis:  Double,
    val lengthOfYear: Double
  ) {

    def fromIntegralYears(years: Int): Epoch =
      fromMilliyears(years * 1000)

    def fromMilliyears(mys: Int): Epoch =
      new Epoch(this, mys)

    def fromLocalDateTime(ldt: LocalDateTime): Epoch =
      fromJulianDay(Scheme.toJulianDay(ldt))

    def fromJulianDay(jd: Double): Epoch =
      fromEpochYears(yearBasis + (jd - julianBasis) / lengthOfYear)

    def fromEpochYears(epochYear: Double): Epoch =
      fromMilliyears((epochYear * 1000.0).toInt)

  }
  object Scheme {

    /**
     * Convert a `LocalDateTime` to a fractional Julian day.
     * @see The Wikipedia [[https://en.wikipedia.org/wiki/Julian_day article]]
     */
    def toJulianDay(dt: LocalDateTime): Double =
      JulianDate.ofLocalDateTime(dt).dayNumber.toDouble

    implicit val SchemeOrder: Order[Scheme] =
      Order.by(s => (s.prefix, s.yearBasis, s.julianBasis, s.lengthOfYear))

    implicit val SchemeShow: Show[Scheme] =
      Show.fromToString

  }

  /**
   * Module of constructors for Besselian epochs.
   * @group Constructors
   */
  case object Besselian extends Scheme('B', 1900.0, 2415020.31352, 365.242198781)

  /**
   * Module of constructors for Julian epochs.
   * @group Constructors
   */
  case object Julian extends Scheme('J', 2000.0, 2451545.0, 365.25)

  implicit val EpochOrder: Order[Epoch] =
    Order.by(e => (e.scheme, e.toMilliyears))

  implicit val EpochShow: Show[Epoch] =
    Show.fromToString

}

trait EpochOptics { this: Epoch.type =>

  val fromString: Format[String, Epoch] =
    Format(
      s => EpochParsers.epoch.parseExact(s),
      e => f"${e.scheme.prefix}%s${e.toMilliyears / 1000}%d.${e.toMilliyears % 1000}%03d"
    )

}
