// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package math

import cats.{ Eq, Order, Show }
import cats.kernel.CommutativeGroup
import cats.instances.long._
import cats.syntax.eq._
import gem.parser.AngleParsers
import gem.syntax.parser._
import gem.optics._
import monocle.{ Iso, Prism }

/**
 * Exact angles represented as integral microarcseconds. These values form a commutative group over
 * addition, where the inverse is reflection around the 0-180° axis. The subgroup of angles where
 * integral microarcseconds correspond with clock microseconds (i.e., where they are evenly
 * divisible by 15 microarcseconds) is represented by the HourAgle subtype.
 *
 * Lawful conversion to and from other types/scales is provided by optics defined on the companion
 * object. Floating-point conversions are provided directly
 * @param toMicroarcseconds This angle in microarcseconds. Exact.
 */
sealed class Angle protected (val toMicroarcseconds: Long) {

  // Sanity checks … should be correct via the companion constructor.
  assert(toMicroarcseconds >= 0, s"Invariant violated. $toMicroarcseconds is negative.")
  assert(toMicroarcseconds < 360L * 60L * 60L * 1000L * 1000L, s"Invariant violated. $toMicroarcseconds is >= 360°.")

  /**
   * Flip this angle 180°. Equivalent to `mirrorBy(this + Angle90)`. Exact, invertible.
   * @group Transformations
   */
  def flip: Angle =
    this + Angle.Angle180

  /**
   * Additive inverse of this angle, equvalent to `mirrorBy Angle0`. Exact, invertible.
   * @group Transformations
   */
  def unary_- : Angle =
    Angle.fromMicroarcseconds(-toMicroarcseconds.toLong)

  /**
   * Mirror image of this angle, when the mirror stands at angle `a`; or picture picking up the
   * circle and flipping it over, around a line drawn from the center going off in direction `a`.
   * So `(88° mirrorBy 90°) = 92°` for instance, as is `88° mirrorBy 270°` since it's the same
   * line. This operation is specified completely by the identity `b - a = (a mirrorBy b) - b`.
   * @group Transformations
   */
  def mirrorBy(a: Angle): Angle = {
    val Δ = a.toMicroarcseconds - toMicroarcseconds
    Angle.fromMicroarcseconds(toMicroarcseconds + Δ * 2L)
  }

  /**
   * This angle in decimal degrees. Approximate, non-invertible.
   * @group Conversions
   */
  def toDoubleDegrees: Double =
    toMicroarcseconds.toDouble / (60.0 * 60.0 * 1000.0 * 1000.0)

  /**
   * This angle in signed decimal degrees. Approximate, non-invertible
   * @group Conversions
   */
  def toSignedDoubleDegrees: Double =
    Angle.signedMicroarcseconds.get(this).toDouble / (60.0 * 60.0 * 1000.0 * 1000.0)

  /**
   * This angle in decimal radian, [0 .. 2π) Approximate, non-invertible
   * @group Conversions
   */
  def toDoubleRadians: Double =
    toDoubleDegrees.toRadians

  /**
   * This angle in signed decimal radians, [-π .. π) Approximate, non-invertible
   * @group Conversions
   */
  def toSignedDoubleRadians: Double =
    toSignedDoubleDegrees.toRadians

  /**
   * Sum of this angle and `a`. Exact, commutative, invertible.
   * @group Operations
   */
  def +(a: Angle): Angle =
    Angle.fromMicroarcseconds(toMicroarcseconds + a.toMicroarcseconds)

  /**
   * Difference of this angle and `a`. Exact, invertible.
   * @group Operations
   */
  def -(a: Angle): Angle =
    Angle.fromMicroarcseconds(toMicroarcseconds - a.toMicroarcseconds)

  /** String representation of this Angle, for debugging purposes only. */
  override def toString =
    f"Angle(${Angle.dms.get(this)}, $toDoubleDegrees%1.10f°)"

  /** Angles are equal if their magnitudes are equal. Exact. */
  override final def equals(a: Any) =
    a match {
      case a: Angle => a.toMicroarcseconds === toMicroarcseconds
      case _        => false
    }

  override final def hashCode =
    toMicroarcseconds.toInt

}

object Angle extends AngleOptics {

  /** @group Constants */ lazy val Angle0:   Angle = degrees.reverseGet(0)
  /** @group Constants */ lazy val Angle90:  Angle = degrees.reverseGet(90)
  /** @group Constants */ lazy val Angle180: Angle = degrees.reverseGet(180)
  /** @group Constants */ lazy val Angle270: Angle = degrees.reverseGet(270)

  /**
   * Construct a new Angle of the given magnitude in integral microarcseconds, modulo 360°. Exact.
   * @group Constructors
   */
  def fromMicroarcseconds(µas: Long): Angle = {
    val µasPer360 = 360L * 60L * 60L * 1000L * 1000L
    val µasʹ = (((µas % µasPer360) + µasPer360) % µasPer360)
    new Angle(µasʹ)
  }

  /**
   * Construct a new Angle of the given magnitude in double degrees, modulo 360°. Approximate.
   * @group Constructors
   */
  def fromDoubleDegrees(ds: Double): Angle =
    fromMicroarcseconds((ds * 60 * 60 * 1000 * 1000).toLong)

  /**
   * Construct a new Angle of the given magnitude in double arcseconds, modulo 360°. Approximate.
   * @group Constructors
   */
  def fromDoubleArcseconds(as: Double): Angle =
    fromMicroarcseconds((as * 1000 * 1000).toLong)

  /**
   * Construct a new Angle of the given magnitude in radians, modulo 2π. Approximate.
   * @group Constructors
   */
  def fromDoubleRadians(rad: Double): Angle =
    fromDoubleDegrees(rad.toDegrees)

  /**
   * Angle forms a commutative group.
   * @group Typeclass Instances
   */
  implicit val AngleCommutativeGroup: CommutativeGroup[Angle] =
    new CommutativeGroup[Angle] {
      val empty: Angle = Angle0
      def combine(a: Angle, b: Angle) = a + b
      def inverse(a: Angle) = -a
    }

  /** @group Typeclass Instances */
  implicit val AngleShow: Show[Angle] =
    Show.fromToString

  /**
   * Angles are equal if their magnitudes are equal.
   * @group Typeclass Instances
   */
  implicit val AngleEqual: Eq[Angle] =
    Eq.fromUniversalEquals

  /**
   * Sorts Angle by magnitude [0, 360) degrees. Not implicit.
   * @group Typeclass Instances
   */
  val AngleOrder: Order[Angle] =
    Order.by(_.toMicroarcseconds)

  /**
   * Sorts Angle by signed angle, so [-180, 180).  Not implicit.
   * @group Typeclass Instances
   */
  val SignedAngleOrder: Order[Angle] =
    Order.by(signedMicroarcseconds.get)

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
    def format: String = f"$degrees%02d:$arcminutes%02d:$arcseconds%02d.$milliarcseconds%03d$microarcseconds%03d"
    override final def toString = s"DMS($format)"
  }
  object DMS {
    implicit val eqDMS: Eq[DMS] =
      Eq.by(_.toAngle)
  }

  /**
   * Construct a new Angle of the given magnitude as a sum of degrees, arcminutes, arcseconds,
   * milliarcseconds, and microarcseconds. Exact modulo 360°.
   * @group Constructors
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

trait AngleOptics extends OpticsHelpers { this: Angle.type =>

  /**
   * Microarcseconds, in [0, 360°).
   * @group Optics
   */
  lazy val microarcseconds: SplitMono[Angle, Long] =
    SplitMono(_.toMicroarcseconds, Angle.fromMicroarcseconds)

  /**
   * Signed microarcseconds, exact, in [-180°, 180°).
   * @group Optics
   */
  lazy val signedMicroarcseconds: SplitMono[Angle, Long] = {
    lazy val µas360 = Angle.Angle180.toMicroarcseconds * 2L
    microarcseconds.imapB[Long](
      identity,
      µas => if (µas >= Angle.Angle180.toMicroarcseconds) µas - µas360  else µas
    )
  }

  // Exact signed angles, scaled by moving the decimal point `scale` digits to the left
  private def signedMicroarcsecondsScaled(scale: Int): SplitMono[Angle, BigDecimal] =
    signedMicroarcseconds.imapB(_.underlying.movePointRight(scale).longValue, n => new java.math.BigDecimal(n).movePointLeft(scale))

  /**
   * Signed decimal milliarcseconds, exact, in [-180°, 180°).
   * @group Optics
   */
  lazy val signedMilliarcseconds: SplitMono[Angle, BigDecimal] =
    signedMicroarcsecondsScaled(3)

  /**
   * Signed decimal arcseconds, exact, in [-180°, 180°).
   * @group Optics
   */
  lazy val signedArcseconds: SplitMono[Angle, BigDecimal] =
    signedMicroarcsecondsScaled(6)

  /**
   * Milliarcseconds, in [0, 360°).
   * @group Optics
   */
  lazy val milliarcseconds: Wedge[Angle, Int] =
    microarcseconds.scaled(1000L)

  /**
   * Arcseconds, in [0, 360°).
   * @group Optics
   */
  lazy val arcseconds: Wedge[Angle, Int] =
    microarcseconds.scaled(1000L * 1000L)

  /**
   * Arcminutes, in [0, 360°).
   * @group Optics
   */
  lazy val arcminutes: Wedge[Angle, Int] =
      microarcseconds.scaled(1000L * 1000L * 60L)

  /**
   * Degrees, in [0, 360).
   * @group Optics
   */
  lazy val degrees: Wedge[Angle, Int] = microarcseconds.scaled(1000L * 1000L * 60L * 60L)

  /**
   * This angle, rounded down to the nearest HourAngle.
   * @group Optics
   */
  lazy val hourAngle: SplitEpi[Angle, HourAngle] =
    SplitEpi(a => HourAngle.microseconds.reverseGet(a.toMicroarcseconds.toLong / 15L), identity)

  /**
   * This angle as an HourAngle, where defined.
   * @group Optics
   */
  lazy val hourAngleExact: Prism[Angle, HourAngle] =
    Prism((a: Angle) => if (a.toMicroarcseconds % 15L === 0L) Some(hourAngle.get(a)) else None)(identity)

  /**
   * This angle as an DMS.
   * @group Optics
   */
  lazy val dms: Iso[Angle, DMS] =
    Iso(DMS(_))(_.toAngle)

  /**
   * String parsed as unsigned DMS.
   * @see [[gem.parser.AngleParsers]]
   * @group Optics
   */
  lazy val fromStringDMS: Format[String, Angle] =
    Format(AngleParsers.dms.parseExact, dms.get(_).format)

  /**
   * String parsed as signed DMS.
   * @see [[gem.parser.AngleParsers]]
   * @group Optics
   */
  lazy val fromStringSignedDMS: Format[String, Angle] =
    Format(fromStringDMS.getOption, { a =>
      if (signedMicroarcseconds.get(a) < 0) "-" + fromStringDMS.reverseGet(-a)
      else "+" + fromStringDMS.reverseGet(a)
    })

}


/**
 * Exact hour angles represented as integral microseconds. These values form a commutative group
 * over addition, where the inverse is reflection around the 0-12h axis. This is a subgroup of the
 * integral Angles where microarcseconds are evenly divisible by 15.
 *
 * Lawful conversion to and from other types/scales is provided by optics defined on the companion
 * object. Floating-point conversions are provided directly
 * @see The helpful [[https://en.wikipedia.org/wiki/Hour_angle Wikipedia]] article.
 */
final class HourAngle private (µas: Long) extends Angle(µas) {

  // Sanity checks … should be correct via the companion constructor.
  assert(toMicroarcseconds %  15 === 0, s"Invariant violated. $µas isn't divisible by 15.")

  /**
   * Flip this HourAngle by 12h. This is logically identical to the superclass implementation
   * and serves only to refine the return type. Exact, invertible.
   * @group Transformations
   */
  override def flip: HourAngle =
    this + HourAngle.HourAngle12

  /**
   * Additive inverse of this HourAngle (by mirroring around the 0-12h axis). This is logically
   * identical to the superclass implementation and serves only to refine the return type. Exact,
   * invertible.
   * @group Transformations
   */
  override def unary_- : HourAngle =
    HourAngle.fromMicroseconds(-toMicroseconds.toLong)

  /**
   * This `HourAngle` in microseconds. Exact.
   * @group Conversions
   */
  def toMicroseconds: Long =
    toMicroarcseconds / 15

  /**
   * This `HourAngle` in decimal hours. Approximate.
   * @group Conversions
   */
  def toDoubleHours: Double =
    toMicroseconds.toDouble / HourAngle.µsPerHour

  /**
   * Sum of this HourAngle and `ha`. Exact, commutative, invertible.
   * @group Operations
   */
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def +(ha: HourAngle): HourAngle =
    HourAngle.fromMicroseconds(toMicroseconds.toLong + ha.toMicroseconds.toLong)

  /**
   * Difference of this HourAngle and `ha`. Exact, invertible.
   * @group Operations
   */
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def -(ha: HourAngle): HourAngle =
    HourAngle.fromMicroseconds(toMicroseconds.toLong - ha.toMicroseconds.toLong)

  /** String representation of this HourAngle, for debugging purposes only. */
  override def toString =
    HourAngle.fromStringHMS.taggedToString("HourAngle", this)

}

object HourAngle extends HourAngleOptics {

  private val µsPerHour: Long = 60L * 60L * 1000L * 1000L

  /** @group Constants */ lazy val HourAngle0 : HourAngle = microseconds.reverseGet(0)
  /** @group Constants */ lazy val HourAngle12: HourAngle = hours.reverseGet(12)

  /**
   * Construct a new Angle of the given magnitude in integral microseconds, modulo 24h. Exact.
   * @group Constructors
   */
  def fromMicroseconds(µs: Long): HourAngle = {
    val µsPer24 = 24L * µsPerHour
    val µsʹ = (((µs % µsPer24) + µsPer24) % µsPer24)
    new HourAngle(µsʹ * 15L)
  }

  /**
   * Construct a new Angle of the given magnitude in floating point hours, modulo 24h. Approximate.
   * @group Constructors
   */
  def fromDoubleHours(hs: Double): HourAngle =
    fromMicroseconds((hs * µsPerHour).round)

  /**
   * Construct a new HourAngle of the given magnitude as a sum of hours, minutes, seconds,
   * milliseconds, and microseconds. Exact modulo 24h.
   * @group Constructors
   */
  def fromHMS(hours: Int, minutes: Int, seconds: Int, milliseconds: Int, microseconds: Int): HourAngle =
    fromMicroseconds(
      microseconds.toLong +
      milliseconds.toLong * 1000L +
      seconds.toLong      * 1000L * 1000L +
      minutes.toLong      * 1000L * 1000L * 60L +
      hours.toLong        * 1000L * 1000L * 60L * 60L
    )

  /**
   * HourAngle forms a commutative group.
   * @group Typeclass Instances
   */
  implicit val AngleCommutativeGroup: CommutativeGroup[HourAngle] =
    new CommutativeGroup[HourAngle] {
      val empty: HourAngle = HourAngle0
      def combine(a: HourAngle, b: HourAngle) = a + b
      def inverse(a: HourAngle) = -a
    }

  /** @group Typeclass Instances */
  implicit val HourAngleShow: Show[HourAngle] =
    Show.fromToString

  /**
   * Angles are equal if their magnitudes are equal.
   * @group Typeclass Instances
   */
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
    def format: String = f"$hours%02d:$minutes%02d:$seconds%02d.$milliseconds%03d$microseconds%03d"
    override final def toString = format
  }
  object HMS {
    implicit val eqHMS: Eq[HMS] =
      Eq.by(_.toHourAngle)
  }

}

trait HourAngleOptics extends OpticsHelpers { this: HourAngle.type =>

  /**
   * This `HourAngle` as an `Angle`.
   * @group Optics
   */
  lazy val angle: SplitMono[HourAngle, Angle] = Angle.hourAngle.reverse

  /**
   * This `HourAngle` in microseconds.
   * @group Optics
   */
  lazy val microseconds: SplitMono[HourAngle, Long] =
    SplitMono(_.toMicroseconds, HourAngle.fromMicroseconds)

  /**
   * This `HourAngle` in milliseconds.
   * @group Optics
   */
  lazy val milliseconds: Wedge[HourAngle, Int] =
    microseconds.scaled(1000L)

  /**
   * This `HourAngle` in seconds.
   * @group Optics
   */
  lazy val seconds: Wedge[HourAngle, Int] =
    microseconds.scaled(1000L * 1000L)

  /**
   * This `HourAngle` in minutes.
   * @group Optics
   */
  lazy val minutes: Wedge[HourAngle, Int] =
    microseconds.scaled(1000L * 1000L * 60L)

  /**
   * This `HourAngle` in hours.
   * @group Optics
   */
  lazy val hours: Wedge[HourAngle, Int] =
    microseconds.scaled(1000L * 1000L * 60L * 60L)

  /**
   * This `HourAngle` as an `HMS`.
   * @group Optics
   */
  lazy val hms: Iso[HourAngle, HMS] =
    Iso(HMS(_))(_.toHourAngle)

  /**
   * String in HMS as an `HourAngle`/
   * @group Optics
   */
  lazy val fromStringHMS: Format[String, HourAngle] =
    Format(AngleParsers.hms.parseExact, HMS(_).format)

}

trait OpticsHelpers {

  // Syntax to scale down and squeeze into Int
  protected implicit class SplitMonoOps[A](self: SplitMono[A, Long]) {

    private val longToInt: SplitEpi[Long, Int] =
      SplitEpi(_.toInt, _.toLong)

    def scaled(n: Long): Wedge[A, Int] =
      self.imapB[Long](_ * n, _ / n) composeSplitEpi longToInt

  }

}