// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package math

import cats.{ Order, Show }
import cats.instances.long._
import gem.syntax.all._
import gem.optics.Format
import monocle.Prism

/**
 * Celestial latitude, measured in angular distance from the celestial equator. Points north of the
 * celestial equator have positive declinations, while those south have negative declinations. This
 * is a newtype wrapper for an `Angle` constrainted to the range [-90°, 90°], or [270 - 360) +
 * [0 - 90] in terms of the underlying `Angle`. Note that the range is *inclusive* of both poles.
 * @see The helpful [[https://en.wikipedia.org/wiki/Declination Wikipedia]] article.
 */
sealed abstract case class Declination protected (toAngle: Angle) {

  // Sanity check … should be correct via the companion constructor.
  assert(
    toAngle.toMicroarcseconds >= Angle.Angle270.toMicroarcseconds ||
    toAngle.toMicroarcseconds <= Angle.Angle90.toMicroarcseconds,
    s"Invariant violated. $toAngle is outside the range [270 - 360) + [0 - 90]"
  )

  /**
   * Offset this [[Declination]] by the given angle, returning the result and a carry bit. A carry
   * of `true` indicates that the result lies on the opposite side of the sphere and the
   * associated [[RightAscension]] (if any) must be flipped by around the 90° axis. Exact,
   * invertible by offseting again by `-a` if carry is false, or by `a` if true; new carry will be
   * the same.
   * @group Operations
   */
  def offset(a: Angle): (Declination, Boolean) =
    Declination.fromAngleWithCarry(toAngle + a)

  /** This declination in signed radians in [-π/2 .. π/2] */
  def toRadians: Double =
    toAngle.toSignedDoubleRadians

  final override def toString: String =
    Declination.fromStringSignedDMS.taggedToString("Dec", this)

}

object Declination extends DeclinationOptics {

  val Min:  Declination = fromAngle.unsafeGet(Angle.Angle270)
  val Max:  Declination = fromAngle.unsafeGet(Angle.Angle90)
  val Zero: Declination = fromAngle.unsafeGet(Angle.Angle0)

  /**
   * Construct a `Declination` from an `Angle`, mirroring about the 90° axis if out of range and
   * reporting whether or not mirroring was required. This operation is useful when offsetting might
   * cause coordinates to cross the pole, in which case the associated RA will need to be flipped
   * 180°.
   * @group Constructors
   */
  def fromAngleWithCarry(a: Angle): (Declination, Boolean) =
    fromAngle.getOption(a).map((_, false)) getOrElse {
      (fromAngle.unsafeGet(a mirrorBy Angle.Angle90), true)
    }

  def fromRadians(rad: Double): Option[Declination] =
    fromAngle.getOption(Angle.fromDoubleRadians(rad))

  def unsafeFromRadians(rad: Double): Declination =
    fromAngle.unsafeGet(Angle.fromDoubleRadians(rad))

  /**
   * Declinations are ordered from south to north.
   * @group Typeclass Instances
   */
  implicit val DeclinationOrder: Order[Declination] =
    Order.by(dec => Angle.signedMicroarcseconds.get(dec.toAngle))

  implicit val DeclinationShow: Show[Declination] =
    Show.fromToString

}

trait DeclinationOptics { this: Declination.type =>

  val fromAngle: Prism[Angle, Declination] =
    Prism((a: Angle) => {
      if (a.toMicroarcseconds >= Angle.Angle270.toMicroarcseconds ||
          a.toMicroarcseconds <= Angle.Angle90.toMicroarcseconds) Some(new Declination(a) {})
      else None
    })(_.toAngle)

  val fromStringSignedDMS: Format[String, Declination] =
    Angle.fromStringSignedDMS.composePrism(fromAngle)

}