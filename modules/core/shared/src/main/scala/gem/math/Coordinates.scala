// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package math

import cats._, cats.implicits._
import gem.parser.CoordinateParsers
import gem.optics.Format
import gem.syntax.all._
import monocle.Lens
import monocle.macros._
import scala.math.{ sin, cos, atan2, sqrt }

/** A point in the sky, given right ascension and declination. */
final case class Coordinates(ra: RightAscension, dec: Declination) {

  /**
   * Offset these `Coordinates` by the given deltas, and indicate whether the declination crossed
   * a pole; if so the right ascension will have been flipped 180°.
   * @group Operations
   */
  def offsetWithCarry(dRA: HourAngle, dDec: Angle): (Coordinates, Boolean) =
    dec.offset(dDec) match {
      case (decʹ, false) => (Coordinates(ra.offset(dRA),      decʹ), false)
      case (decʹ, true)  => (Coordinates(ra.flip.offset(dRA), decʹ), true)
    }

  /**
   * Offset these `Coordinates` by the given deltas. If the declination crossed a pole the right
   * ascension will have been flipped 180°.
   * @group Operations
   */
  def offset(dRA: HourAngle, dDec: Angle): Coordinates =
    offsetWithCarry(dRA, dDec)._1

  /**
   * Compute the offset between points, such that `a offset (a diff b) = b`.
   * @group Operations
   */
  def diff(c: Coordinates): (HourAngle, Angle) =
    (c.ra.toHourAngle - ra.toHourAngle, c.dec.toAngle - dec.toAngle)

  /**
   * Angular distance from `this` to `that`, always a positive angle in [0, 180]). Approximate.
   * @see Algorithm at [[http://www.movable-type.co.uk/scripts/latlong.html Movable Type Scripts]].
   */
  def angularDistance(that: Coordinates): Angle = {
    val φ1 = this.dec.toAngle.toDoubleRadians
    val φ2 = that.dec.toAngle.toDoubleRadians
    val Δφ = (that.dec.toAngle - this.dec.toAngle).toDoubleRadians
    val Δλ = (that.ra.toAngle  - this.ra.toAngle) .toDoubleRadians
    val a  = sin(Δφ / 2) * sin(Δφ / 2) +
             cos(φ1)     * cos(φ2)     *
             sin(Δλ / 2) * sin(Δλ / 2)
    Angle.fromDoubleRadians(2 * atan2(sqrt(a), sqrt(1 - a)))
  }

  /**
   * Interpolate between `this` and `that` at a position specified by `f`, where `0.0` is `this`,
   * `1.0` is `other`, and `0.5` is halfway along the great circle connecting them. Note that this
   * computation is undefined where `f` is `NaN` or `Infinity`. Approximate.
   * @see Algorithm at [[http://www.movable-type.co.uk/scripts/latlong.html Movable Type Scripts]].
   */
  def interpolate(that: Coordinates, f: Double): Coordinates = {
    val δ = angularDistance(that).toDoubleRadians
    if (δ === 0) this
    else {
      val φ1 = this.dec.toAngle.toDoubleRadians
      val φ2 = that.dec.toAngle.toDoubleRadians
      val λ1 = this.ra.toAngle.toDoubleRadians
      val λ2 = that.ra.toAngle.toDoubleRadians
      val a  = sin((1 - f) * δ) / sin(δ) // n.b. this line is wrong on the web page
      val b  = sin(f * δ) / sin(δ)
      val x  = a * cos(φ1) * cos(λ1) + b * cos(φ2) * cos(λ2)
      val y  = a * cos(φ1) * sin(λ1) + b * cos(φ2) * sin(λ2)
      val z  = a * sin(φ1) + b * sin(φ2)
      val φi = atan2(z, sqrt(x * x + y * y))
      val λi = atan2(y, x)
      Coordinates(
        RA.fromHourAngle.get(Angle.hourAngle.get(Angle.fromDoubleRadians(λi))),
        Dec.fromAngle.unsafeGet(Angle.fromDoubleRadians(φi))
      )
    }
  }

  /** These coordinates in radians, [0 .. 2π) and [-π/2 .. π/2]. */
  def toRadians: (Double, Double) =
    (ra.toRadians, dec.toRadians)

  override def toString =
    Coordinates.fromHmsDms.productToString(this)

}

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object Coordinates extends CoordinatesOptics {

  /* @group Constructors */ val Zero:      Coordinates = Coordinates(RA.Zero, Dec.Zero)
  /* @group Constructors */ val SouthPole: Coordinates = Coordinates(RA.Zero, Dec.Min)
  /* @group Constructors */ val NorthPole: Coordinates = Coordinates(RA.Zero, Dec.Max)

  def fromRadians(ra: Double, dec: Double): Option[Coordinates] =
    Declination.fromRadians(dec).map(Coordinates(RA.fromRadians(ra), _))

  def unsafeFromRadians(ra: Double, dec: Double): Coordinates =
    Coordinates(RA.fromRadians(ra), Declination.unsafeFromRadians(dec))

  /** @group Typeclass Instances */
  implicit val CoordinatesOrder: Order[Coordinates] =
    Order.by(c => (c.ra, c.dec))

  /** @group Typeclass Instances. */
  implicit val ShowCoordinates: Show[Coordinates] =
    Show.fromToString

}

trait CoordinatesOptics { this: Coordinates.type =>

  /**
   * Format as a String like "17 57 48.49803 +04 41 36.2072".
   * @group Optics
   */
  val fromHmsDms: Format[String, Coordinates] = Format(
    CoordinateParsers.coordinates.parseExact,
    cs => s"${RightAscension.fromStringHMS.reverseGet(cs.ra)} ${Declination.fromStringSignedDMS.reverseGet(cs.dec)}"
  )

  /** @group Optics */
  val rightAscension: Lens[Coordinates, RightAscension] =
    GenLens[Coordinates](_.ra)

  /** @group Optics */
  val declination: Lens[Coordinates, Declination] =
    GenLens[Coordinates](_.dec)

}
