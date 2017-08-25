// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.implicits._
import scala.math.{ sin, cos, hypot, atan2 }

/**
 * Time-parameterized coordinates, based on an observed position at some point in time (called
 * the `epoch`) and measured velocities in distance (`redShift`; i.e., Doppler shift) and position
 * (`velocity`) per year.
 * @param base observed coordinates at `epoch`
 * @param epoch time of the base observation; typically `Epoch.J2000`
 * @param pv proper velocity *per year* in [[RightAscension]] and [[Declination]]
 * @param rv radial velocity in km/y, positive if receding
 * @param px parallax
 */
final case class ProperMotion(
  base:  Coordinates,
  epoch: Epoch,
  pv:    Option[Offset],
  rv:    Option[Double], // for now
  px:    Option[Angle]
) {

  def plusYears(dt: Double): Coordinates =
    ProperMotion.properMotion(base, epoch, pv.orEmpty, px.orEmpty, rv.getOrElse(0.0), dt)

}


object ProperMotion {

  /** Proper motion of Barnard's Star, for reference. */
  val Barnard: ProperMotion =
    ProperMotion(
      Coordinates.parse("17 57 48.49803 +04 41 36.2072").getOrElse(sys.error("oops")),
      Epoch.J2000,
      Some(Offset(
        Offset.P(Angle.fromMicroarcseconds( -798580L)),
        Offset.Q(Angle.fromMicroarcseconds(10328120L))
      )),
      Some(-110.6), // km/s
      Some(Angle.fromMicroarcseconds(545620L))
    )

  def main(args: Array[String]): Unit = {
    (0.0 to 10.0 by 1.0) foreach { y =>
      val cs = Barnard.plusYears(y)
      println(s"${2000 + y} -> $cs")
    }
  }

  // Some constants we need
  private val secsPerDay  = 86400.0
  private val auPerKm     = 1.0 / 149597870.0
  private val radsPerAsec = Angle.fromArcseconds(1).toDoubleRadians
  private val twoPi = 6.283185307179586476925286766559

  private type Vec2 = (Double, Double)
  private type Vec3 = (Double, Double, Double)

  // scalar multiplication
  implicit class Vec3Ops(a: Vec3) {
    def *(d: Double): Vec3 =
      (a._1 * d, a._2 * d, a._3 * d)
  }

  /**
   * Proper motion correction in model units.
   * @param base base coordinates
   * @param e    the epoch
   * @param pv   proper velocity per epoch year
   * @param px   parallax
   * @param rv   radial velocity (km/sec, positive means away from earth)
   * @param dt   elapsed time in epoch years
   * @return Coordinates corrected for proper motion
   */
  def properMotion(base: Coordinates, e: Epoch, pv: Offset, px: Angle, rv: Double, dt: Double): Coordinates = {
    val (ra, dec) = properMotionʹ(base.toRadians, e.scheme.lengthOfYear, pv.toRadians, px.toMicroarcseconds.toDouble / 1000000.0, rv, dt)
    Coordinates.unsafeFromRadians(ra, dec)
  }

  /**
   * Proper motion correction in base units.
   * @param baseCoordinates base (ra, dec) in radians, [0 … 2π) and (-π/2 … π/2)
   * @param daysPerYear length of epoch year in fractonal days
   * @param properVelocity   proper velocity in (ra, dec) in radians per epoch year
   * @param parallax   parallax in arcseconds (!)
   * @param radialVelocity   radial velocity (km/sec, positive means away from earth)
   * @param elapsedYears   elapsed time in epoch years
   * @return (ra, dec) in radians as given, corrected for proper motion
   */
  private def properMotionʹ(
    baseCoordinates: Vec2,
    daysPerYear:      Double,
    properVelocity:  Vec2,
    parallax:         Double,
    radialVelocity:   Double,
    elapsedYears:     Double
  ): Vec2 = {

    // Break out our components
    val (ra,   dec) = baseCoordinates
    val (dRa, dDec) = properVelocity

    // Convert to cartesian
    val pos: Vec3 = {
      val cd = cos(dec)
      (cos(ra) * cd, sin(ra) * cd, sin(dec))
    }

    // Change per year due to radial velocity and parallax
    val dPos1: Vec3 =
      pos            *
      daysPerYear    *
      secsPerDay     *
      radsPerAsec    *
      auPerKm        *
      radialVelocity *
      parallax

    // Change per year due to proper velocity
    val dPos2 = (
      -dRa * pos._2 - dDec * cos(ra) * sin(dec),
       dRa * pos._1 - dDec * sin(ra) * sin(dec),
                      dDec *           cos(dec)
    )

    // Our new position (still in polar coordinates)
    val pʹ = pos |+| ((dPos1 |+| dPos2) * elapsedYears)

    // Back to spherical
    val (x, y, z) = pʹ
    val r    = hypot(x, y)
    val raʹ  = if (r === 0.0) 0.0 else atan2(y, x)
    val decʹ = if (z === 0.0) 0.0 else atan2(z, r)
    val raʹʹ = {
      // Normalize to [0 .. 2π)
      val rem = raʹ % twoPi
      if (rem < 0.0) rem + twoPi else rem
    }
    (raʹʹ, decʹ)

  }

}
