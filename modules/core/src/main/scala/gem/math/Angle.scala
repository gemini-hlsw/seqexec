// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import scalaz.Monoid

/**
 * Exact angles represented as integral milliarcseconds. These values form an Abelian group over
 * addition, where the inverse is reflection around the 0-180° axis. The subgroup of angles where
 * integral milliarcseconds correspond with clock milliseconds (i.e., where they are evenly
 * divisible by 15 milliarcseconds) is represented by the HourAgle subtype.
 * @param toMilliarcseconds This angle in milliarcseconds. Exact.
 */
sealed class Angle (val toMilliarcseconds: Int) {

  // Sanity checks … should be correct via the companion constructor.
  assert(toMilliarcseconds >= 0, s"Invariant violated. $toMilliarcseconds is negative.")
  assert(toMilliarcseconds < 360 * 60 * 60 * 1000, s"Invariant violated. $toMilliarcseconds is >= 360°.")

  /** Flip this angle 180°. Exact, invertible. */
  def flip: Angle =
    this + Angle.Angle180

  /** Additive inverse of this angle (by mirroring around the 0-180 axis). Exact, invertible. */
  def unary_- : Angle =
    Angle.fromMilliarcseconds(-toMilliarcseconds.toLong)

  /** This angle in decimal degrees. Approximate, non-invertible */
  def toDoubleDegrees: Double =
    toMilliarcseconds.toDouble / (60.0 * 60.0 * 1000.0)

  /** This angle in decimal radisns. Approximate, non-invertible */
  def toDoubleRadians: Double =
    toDoubleDegrees.toRadians

  /**
   * Convert to the closest hour angle by rounding down to the closest 15 milliarcseconds.
   * Exact, non-invertible.
   */
  def toHourAngle: HourAngle =
    HourAngle.fromMilliseconds(toMilliarcseconds.toLong / 15L)

  /**
   * Convert to the closest hour angle iff its magnitide is an even multiple of 15 milliarcseconds.
   * Exact and invertible where defined.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def toHourAngleExact: Option[HourAngle] =
    if (toMilliarcseconds % 15 == 0) Some(toHourAngle) else None

  /**
   * Destructure this value into a sum of degrees, arcminutes, arcseconds, and milliarcseconds.
   * Exact, invertible via `Angle.fromDMS`.
   */
  def toDMS: Angle.DMS =
    Angle.DMS(this)

  /** Sum of this angle and `a`. Exact, commutative, invertible. */
  def +(a: Angle): Angle =
    Angle.fromMilliarcseconds(toMilliarcseconds.toLong + a.toMilliarcseconds.toLong)

  /** Difference of this angle and `a`. Exact, invertible. */
  def -(a: Angle): Angle =
    Angle.fromMilliarcseconds(toMilliarcseconds.toLong - a.toMilliarcseconds.toLong)

  /** String representation of this Angle, for debugging purposes only. */
  override def toString =
    f"Angle($toDMS, $toDoubleDegrees°)"

  /** Angles are equal if their magnitudes are equal. Exact. */
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override final def equals(a: Any) =
    a match {
      case a: Angle => a.toMilliarcseconds == toMilliarcseconds
      case _        => false
    }

  override final def hashCode =
    toMilliarcseconds

}

object Angle {

  val Angle0:   Angle = fromDegrees(0L)
  val Angle90:  Angle = fromDegrees(90L)
  val Angle180: Angle = fromDegrees(180L)

  /** Construct a new Angle of the given magnitide in integral milliarcseconds, modulo 360°. Exact. */
  def fromMilliarcseconds(mas: Long): Angle = {
    val masPer360 = 360 * 60 * 60 * 1000
    val masʹ = (((mas % masPer360) + masPer360) % masPer360)
    new Angle(masʹ.toInt)
  }

  /** Construct a new Angle of the given magnitide in integral arcseconds, modulo 360. Exact. */
  def fromArcseconds(as: Long): Angle = fromMilliarcseconds(as * 1000)

  /** Construct a new Angle of the given magnitide in integral arcminutes, modulo 360. Exact. */
  def fromArcminutes(ms: Long): Angle = fromArcseconds(ms * 60)

  /** Construct a new Angle of the given magnitide in integral degrees, modulo 360. Exact. */
  def fromDegrees(ms: Long):    Angle = fromArcminutes(ms * 60)

  /** Construct a new Angle of the given magnitide in integral degrees, modulo 360°. Approximate. */
  def fromDoubleDegrees(ds: Double): Angle =
    fromMilliarcseconds((ds * 60 * 60 * 1000).toLong)

  /** Construct a new Angle of the given magnitide in radians, modulo 2π. Approximate. */
  def fromDoubleRadians(rad: Double): Angle =
    fromDoubleDegrees(rad.toDegrees)

  /** Angle is an Abelian group, but monoid is the best we can do for now. */
  implicit val AngleMonoid: Monoid[Angle] =
    Monoid.instance(_ + _, Angle.Angle0)

  // This works for both DMS and HMS so let's just do it once.
  protected[math] def toMillisexigesimal(millis: Int): (Int, Int, Int, Int) = {
    val ms =  millis                     % 1000
    val s  = (millis / (1000))           % 60
    val m  = (millis / (1000 * 60))      % 60
    val d  = (millis / (1000 * 60 * 60))
    (d, m, s, ms)
  }

  /**
   * Integral angle represented as a sum of degrees, arcminutes, arcseconds, and milliarcseconds.
   * This type is exact and isomorphic to Angle.
   */
  final case class DMS(toAngle: Angle) {
    val (
      degrees: Int,
      arcminutes: Int,
      arcseconds: Int,
      milliarcseconds: Int
    ) = Angle.toMillisexigesimal(toAngle.toMilliarcseconds)
    override final def toString =
      f"$degrees:$arcminutes%02d:$arcseconds%02d.$milliarcseconds%03d"
  }

  /**
   * Construct a new Angle of the given magnitide as a sum of degrees, arcminutes, arcseconds, and
   * milliarcseconds. Exact modulo 360°.
   */
  def fromDMS(degrees: Int, arcminutes: Int, arcseconds: Int, milliarcseconds: Int): Angle =
    fromMilliarcseconds(
      milliarcseconds.toLong +
      arcseconds.toLong * 1000 +
      arcminutes.toLong * 1000 * 60 +
      degrees.toLong    * 1000 * 60 * 60
    )

}



/**
 * Exact hour angles represented as integral milliseconds. These values form an Abelian group over
 * addition, where the inverse is reflection around the 0-12h axis. This is a subgroup of the
 * integral Angles where milliarcseconds are evenly divisible by 15.
 */
@SuppressWarnings(Array("org.wartremover.warts.Equals"))
final class HourAngle private (mas: Int) extends Angle(mas) {

  // Sanity checks … should be correct via the companion constructor.
  assert(toMilliarcseconds %  15 == 0, s"Invariant violated. $mas isn't divisible by 15.")

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
    HourAngle.fromMilliseconds(-toMilliseconds.toLong)

  // Overridden for efficiency
  override def toHourAngle = this
  override def toHourAngleExact = Some(this)

  // Define in terms of toMilliarcseconds to avoid a second member
  def toMilliseconds: Int =
    toMilliarcseconds / 15

  def toHMS: HourAngle.HMS =
    HourAngle.HMS(this)

  /** Sum of this HourAngle and `ha`. Exact, commutative, invertible. */
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def +(ha: HourAngle): HourAngle =
    HourAngle.fromMilliseconds(toMilliseconds.toLong + ha.toMilliseconds.toLong)

  /** Difference of this HourAngle and `ha`. Exact, invertible. */
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def -(ha: HourAngle): HourAngle =
    HourAngle.fromMilliseconds(toMilliseconds.toLong - ha.toMilliseconds.toLong)

  /** String representation of this HourAngle, for debugging purposes only. */
  override def toString =
    f"HourAngle($toDMS, $toHMS, $toDoubleDegrees°)"

}

object HourAngle {

  val HourAngle0 : HourAngle = fromMilliseconds(0)
  val HourAngle12: HourAngle = fromHours(12)

  /** Construct a new Angle of the given magnitide in integral milliseconds, modulo 24h. Exact. */
  def fromMilliseconds(ms: Long): HourAngle = {
    val msPer24 = 24 * 60 * 60 * 1000
    val msʹ = (((ms % msPer24) + msPer24) % msPer24)
    new HourAngle(msʹ.toInt * 15)
  }

  /** Construct a new HourAngle of the given magnitide in integral milliseconds, modulo 24h. Exact. */
  def fromSeconds(seconds: Long): HourAngle =
    fromMilliseconds(seconds * 1000L)

  /** Construct a new HourAngle of the given magnitide in integral milliseconds, modulo 24h. Exact. */
  def fromMinutes(minutes: Long): HourAngle =
    fromSeconds(minutes * 60L)

  /** Construct a new HourAngle of the given magnitide in integral milliseconds, modulo 24h. Exact. */
  def fromHours(hours: Long):     HourAngle =
    fromMinutes(hours * 60L)

  def fromDoubleHours(hs: Double): HourAngle =
    fromMilliseconds((hs * 60 * 60 * 1000).toLong)

  /** HourAngle is an Abelian group (a subgroup of Angle), but monoid is the best we can do for now. */
  implicit val HourAngleMonoid: Monoid[HourAngle] =
    Monoid.instance(_ + _, HourAngle.HourAngle0)

  /**
   * Integral hour angle represented as a sum of hours, minutes, seconds, and milliseconds. This
   * type is exact and isomorphic to HourAngle.
   */
  final case class HMS(toHourAngle: HourAngle) {
    def toAngle: Angle = toHourAngle // forget it's an hour angle
    val (
      hours: Int,
      minutes: Int,
      seconds: Int,
      milliseconds: Int
    ) = Angle.toMillisexigesimal(toHourAngle.toMilliarcseconds)
    override final def toString =
      f"$hours:$minutes%02d:$seconds%02d.$milliseconds%03d"
  }

}
