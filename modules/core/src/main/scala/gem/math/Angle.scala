// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package math

import cats.{ Eq, Show }
import cats.kernel.CommutativeGroup
import cats.instances.long._
import cats.syntax.eq._

/**
 * Exact angles represented as integral microarcseconds. These values form a commutative group over
 * addition, where the inverse is reflection around the 0-180° axis. The subgroup of angles where
 * integral microarcseconds correspond with clock microseconds (i.e., where they are evenly
 * divisible by 15 microarcseconds) is represented by the HourAgle subtype.
 * @param toMicroarcseconds This angle in microarcseconds. Exact.
 */
sealed class Angle protected (val toMicroarcseconds: Long) {

  // Sanity checks … should be correct via the companion constructor.
  assert(toMicroarcseconds >= 0, s"Invariant violated. $toMicroarcseconds is negative.")
  assert(toMicroarcseconds < 360L * 60L * 60L * 1000L * 1000L, s"Invariant violated. $toMicroarcseconds is >= 360°.")

  /**
   * Flip this angle 180°. Equivalent to `mirrorBy(this + Angle90)`. Exact, invertible.
   */
  def flip: Angle =
    this + Angle.Angle180

  /** Additive inverse of this angle, equvalent to `mirrorBy Angle0`. Exact, invertible. */
  def unary_- : Angle =
    Angle.fromMicroarcseconds(-toMicroarcseconds.toLong)

  /**
   * Mirror image of this angle, when the mirror stands at angle `a`; or picture picking up the
   * circle and flipping it over, around a line drawn from the center going off in direction `a`.
   * So `(88° mirrorBy 90°) = 92°` for instance, as is `88° mirrorBy 270°` since it's the same
   * line. This operation is specified completely by the identity `b - a = (a mirrorBy b) - b`.
   */
  def mirrorBy(a: Angle): Angle = {
    val Δ = a.toMicroarcseconds - toMicroarcseconds
    Angle.fromMicroarcseconds(toMicroarcseconds + Δ * 2L)
  }

  /** Signed microarcseconds, in [-180°, 180°). */
  def toSignedMicroarcseconds: Long = {
    val µas360 = Angle.Angle180.toMicroarcseconds * 2L
    if (toMicroarcseconds >= Angle.Angle180.toMicroarcseconds) toMicroarcseconds - µas360
    else toMicroarcseconds
  }

  /** This angle in decimal degrees. Approximate, non-invertible */
  def toDoubleDegrees: Double =
    toMicroarcseconds.toDouble / (60.0 * 60.0 * 1000.0 * 1000.0)

  /** This angle in decimal radisns. Approximate, non-invertible */
  def toDoubleRadians: Double =
    toDoubleDegrees.toRadians

  /**
   * Convert to the closest hour angle by rounding down to the closest 15 milliarcseconds.
   * Exact, non-invertible.
   */
  def toHourAngle: HourAngle =
    HourAngle.fromMicroseconds(toMicroarcseconds.toLong / 15L)

  /**
   * Convert to the closest hour angle iff its magnitude is an even multiple of 15 milliarcseconds.
   * Exact and invertible where defined.
   */
  def toHourAngleExact: Option[HourAngle] =
    if (toMicroarcseconds % 15L === 0L) Some(toHourAngle) else None

  /**
   * Destructure this value into a sum of degrees, arcminutes, arcseconds, milliarcseconds, and
   * microseconds. Exact, invertible via `Angle.fromDMS`.
   */
  def toDMS: Angle.DMS =
    Angle.DMS(this)

  /** Sum of this angle and `a`. Exact, commutative, invertible. */
  def +(a: Angle): Angle =
    Angle.fromMicroarcseconds(toMicroarcseconds + a.toMicroarcseconds)

  /** Difference of this angle and `a`. Exact, invertible. */
  def -(a: Angle): Angle =
    Angle.fromMicroarcseconds(toMicroarcseconds - a.toMicroarcseconds)

  /** String representation of this Angle, for debugging purposes only. */
  override def toString =
    f"Angle($toDMS, $toDoubleDegrees%1.10f°)"

  /** Angles are equal if their magnitudes are equal. Exact. */
  override final def equals(a: Any) =
    a match {
      case a: Angle => a.toMicroarcseconds === toMicroarcseconds
      case _        => false
    }

  override final def hashCode =
    toMicroarcseconds.toInt

  /** Format this angle as a human-readable DMS string. Invertable via `Angle.unformatDMS`. */
  def formatDMS: String =
    toDMS.format

  /**
   * Format this angle as a human-readable signed [180°, 180°) DMS string. Invertable via
   * `Angle.unformatSignedDMS`.
   */
  def formatSignedDMS: String =
    if (toSignedMicroarcseconds < 0) "-" + unary_-.formatDMS
    else  "+" + formatDMS

}

object Angle {

  val Angle0:   Angle = fromDegrees(0)
  val Angle90:  Angle = fromDegrees(90)
  val Angle180: Angle = fromDegrees(180)
  val Angle270: Angle = fromDegrees(270)

  /** Construct a new Angle of the given magnitude in integral microarcseconds, modulo 360°. Exact. */
  def fromMicroarcseconds(µas: Long): Angle = {
    val µasPer360 = 360L * 60L * 60L * 1000L * 1000L
    val µasʹ = (((µas % µasPer360) + µasPer360) % µasPer360)
    new Angle(µasʹ)
  }

  /** Construct a new Angle of the given magnitude in integral arcseconds, modulo 360°. Exact. */
  def fromMilliarcseconds(as: Int): Angle =
    fromMicroarcseconds(as.toLong * 1000L)

  /** Construct a new Angle of the given magnitude in integral arcseconds, modulo 360°. Exact. */
  def fromArcseconds(as: Int): Angle =
    fromMilliarcseconds(as * 1000)

  /** Construct a new Angle of the given magnitude in integral arcminutes, modulo 360°. Exact. */
  def fromArcminutes(ms: Int): Angle =
    fromArcseconds(ms * 60)

  /** Construct a new Angle of the given magnitude in integral degrees, modulo 360°. Exact. */
  def fromDegrees(ms: Int): Angle =
    fromArcminutes(ms * 60)

  /** Construct a new Angle of the given magnitude in double degrees, modulo 360°. Approximate. */
  def fromDoubleDegrees(ds: Double): Angle =
    fromMicroarcseconds((ds * 60 * 60 * 1000 * 1000).toLong)

  /** Construct a new Angle of the given magnitude in double arcseconds, modulo 360°. Approximate. */
  def fromDoubleArcseconds(as: Double): Angle =
    fromMicroarcseconds((as * 1000 * 1000).toLong)

  /** Construct a new Angle of the given magnitude in radians, modulo 2π. Approximate. */
  def fromDoubleRadians(rad: Double): Angle =
    fromDoubleDegrees(rad.toDegrees)

  /** Attempt to parse an [[Angle]] from a `.formatDMS`-formatted string. */
  def unformatDMS(s: String): Option[Angle] =
    Parsers.parseExact(Parsers.angle.hms)(s) // N.B. this parser is too lenient; it should reject signed angles

  /** Attempt to parse an [[Angle]] from a `.formatSignedDMS`-formatted string. */
  def unformatSignedDMS(s: String): Option[Angle] =
    Parsers.parseExact(Parsers.angle.hms)(s)

  /** Angle forms a commutative group. */
  implicit val AngleCommutativeGroup: CommutativeGroup[Angle] =
    new CommutativeGroup[Angle] {
      val empty: Angle = Angle0
      def combine(a: Angle, b: Angle) = a + b
      def inverse(a: Angle) = -a
    }

  implicit val AngleShow: Show[Angle] =
    Show.fromToString

  /** Angles are equal if their magnitudes are equal. */
  implicit val AngleEqual: Eq[Angle] =
    Eq.fromUniversalEquals

  // This works for both DMS and HMS so let's just do it once.
  protected[math] def toMicrosexigesimal(micros: Long): (Int, Int, Int, Int, Int) = {
    val µs =  micros                               % 1000L
    val ms = (micros / (1000L))                    % 1000L
    val s  = (micros / (1000L * 1000L))            % 60L
    val m  = (micros / (1000L * 1000L * 60L))      % 60L
    val d  = (micros / (1000L * 1000L * 60L * 60L))
    (d.toInt, m.toInt, s.toInt, ms.toInt, µs.toInt)
  }

  /**
   * Integral angle represented as a sum of degrees, arcminutes, arcseconds, milliarcseconds and
   * microarcseconds. This type is exact and isomorphic to Angle.
   */
  final case class DMS(toAngle: Angle) {
    val (
      degrees: Int,
      arcminutes: Int,
      arcseconds: Int,
      milliarcseconds: Int,
      microarcseconds: Int
    ) = Angle.toMicrosexigesimal(toAngle.toMicroarcseconds)
    def format: String = f"$degrees° $arcminutes%02d′ $arcseconds%02d.$milliarcseconds%03d$microarcseconds%03d″"
    override final def toString = s"DMS($format)"
  }

  /**
   * Construct a new Angle of the given magnitude as a sum of degrees, arcminutes, arcseconds,
   * milliarcseconds, and microarcseconds. Exact modulo 360°.
   */
  def fromDMS(
    degrees:         Int,
    arcminutes:      Int,
    arcseconds:      Int,
    milliarcseconds: Int,
    microarcseconds: Int
  ): Angle =
    fromMicroarcseconds(
      microarcseconds.toLong +
      milliarcseconds.toLong * 1000 +
      arcseconds.toLong      * 1000 * 1000 +
      arcminutes.toLong      * 1000 * 1000 * 60 +
      degrees.toLong         * 1000 * 1000 * 60 * 60
    )

}



/**
 * Exact hour angles represented as integral microseconds. These values form a commutative group
 * over addition, where the inverse is reflection around the 0-12h axis. This is a subgroup of the
 * integral Angles where microarcseconds are evenly divisible by 15.
 * @see The helpful [[https://en.wikipedia.org/wiki/Hour_angle Wikipedia]] article.
 */
final class HourAngle private (µas: Long) extends Angle(µas) {

  // Sanity checks … should be correct via the companion constructor.
  assert(toMicroarcseconds %  15 === 0, s"Invariant violated. $µas isn't divisible by 15.")

  /** Forget this is an HourAngle. */
  def toAngle: Angle =
    this

  /**
   * Flip this HourAngle by 12h. This is logically identical to the superclass implementation
   * and serves only to refine the return type. Exact, invertible.
   */
  override def flip: HourAngle =
    this + HourAngle.HourAngle12

  /**
   * Additive inverse of this HourAngle (by mirroring around the 0-12h axis). This is logically
   * identical to the superclass implementation and serves only to refine the return type. Exact,
   * invertible.
   */
  override def unary_- : HourAngle =
    HourAngle.fromMicroseconds(-toMicroseconds.toLong)

  // Overridden for efficiency
  override def toHourAngle = this
  override def toHourAngleExact = Some(this)

  // Define in terms of toMicroarcseconds to avoid a second member
  def toMicroseconds: Long =
    toMicroarcseconds / 15

  def toHMS: HourAngle.HMS =
    HourAngle.HMS(this)

  /** Sum of this HourAngle and `ha`. Exact, commutative, invertible. */
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def +(ha: HourAngle): HourAngle =
    HourAngle.fromMicroseconds(toMicroseconds.toLong + ha.toMicroseconds.toLong)

  /** Difference of this HourAngle and `ha`. Exact, invertible. */
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def -(ha: HourAngle): HourAngle =
    HourAngle.fromMicroseconds(toMicroseconds.toLong - ha.toMicroseconds.toLong)

  /** Format this angle as a human-readable HMS string. Invertable via `Angle.unformatDMS`. */
  def formatHMS: String =
    toHMS.format

  /** String representation of this HourAngle, for debugging purposes only. */
  override def toString =
    f"HourAngle($formatHMS)"

}

object HourAngle {

  val HourAngle0 : HourAngle = fromMicroseconds(0)
  val HourAngle12: HourAngle = fromHours(12)

  /** Construct a new Angle of the given magnitude in integral microseconds, modulo 24h. Exact. */
  def fromMicroseconds(µs: Long): HourAngle = {
    val µsPer24 = 24L * 60L * 60L * 1000L * 1000L
    val µsʹ = (((µs % µsPer24) + µsPer24) % µsPer24)
    new HourAngle(µsʹ * 15L)
  }

  /** Construct a new HourAngle of the given magnitude in integral milliseconds, modulo 24h. Exact. */
  def fromMilliseconds(milliseconds: Int): HourAngle =
    fromMicroseconds(milliseconds.toLong * 1000L)

  /** Construct a new HourAngle of the given magnitude in integral seconds, modulo 24h. Exact. */
  def fromSeconds(seconds: Int): HourAngle =
    fromMilliseconds(seconds * 1000)

  /** Construct a new HourAngle of the given magnitude in integral minutes, modulo 24h. Exact. */
  def fromMinutes(minutes: Int): HourAngle =
    fromSeconds(minutes * 60)

  /** Construct a new HourAngle of the given magnitude in integral hours, modulo 24h. Exact. */
  def fromHours(hours: Int):     HourAngle =
    fromMinutes(hours * 60)

  def fromDoubleHours(hs: Double): HourAngle =
    fromMicroseconds((hs * 60.0 * 60.0 * 1000.0).toLong)

  /**
   * Construct a new HourAngle of the given magnitude as a sum of hours, minutes, seconds,
   * milliseconds, and microseconds. Exact modulo 24h.
   */
  def fromHMS(hours: Int, minutes: Int, seconds: Int, milliseconds: Int, microseconds: Int): HourAngle =
    fromMicroseconds(
      microseconds.toLong +
      milliseconds.toLong * 1000L +
      seconds.toLong      * 1000L * 1000L +
      minutes.toLong      * 1000L * 1000L * 60L +
      hours.toLong        * 1000L * 1000L * 60L * 60L
    )


  /** Attempt to parse an [[HourAngle]] from a `.formatHMS`-formatted string. */
  def unformatHMS(s: String): Option[HourAngle] =
    Parsers.parseExact(Parsers.angle.hms)(s)

  /** HourAngle forms a commutative group. */
  implicit val AngleCommutativeGroup: CommutativeGroup[HourAngle] =
    new CommutativeGroup[HourAngle] {
      val empty: HourAngle = HourAngle0
      def combine(a: HourAngle, b: HourAngle) = a + b
      def inverse(a: HourAngle) = -a
    }

  implicit val HourAngleShow: Show[HourAngle] =
    Show.fromToString

  /** Angles are equal if their magnitudes are equal. */
  implicit val HourAngleEqual: Eq[HourAngle] =
    Eq.by(_.toMicroarcseconds)

  /**
   * Integral hour angle represented as a sum of hours, minutes, seconds, milliseconds, and
   * microseconds. This type is exact and isomorphic to HourAngle.
   */
  final case class HMS(toHourAngle: HourAngle) {
    def toAngle: Angle = toHourAngle // forget it's an hour angle
    val (
      hours: Int,
      minutes: Int,
      seconds: Int,
      milliseconds: Int,
      microseconds: Int
    ) = Angle.toMicrosexigesimal(toHourAngle.toMicroseconds)
    def format: String = f"${hours}h $minutes%02dm $seconds%02d.$milliseconds%03d$microseconds%03ds"
    override final def toString = format
  }

}
