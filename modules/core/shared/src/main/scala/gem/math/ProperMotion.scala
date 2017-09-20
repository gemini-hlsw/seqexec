// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.implicits._
import java.time.Instant
import scala.math.{ sin, cos, hypot, atan2 }

/**
 * Time-parameterized coordinates, based on an observed position at some point in time (called
 * the `epoch`) and measured velocities in distance (`radialVelocity`; i.e., doppler shift) and
 * position (`properVelocity`) per year. Given this information we can compute the position at any
 * instant in time. The references below are ''extremely'' helpful, so do check them out if you're
  * trying to understand the implementation.
 * @see The pretty good [[https://en.wikipedia.org/wiki/Proper_motion wikipedia]] article
 * @see Astronomical Almanac 1984 [[https://babel.hathitrust.org/cgi/pt?id=uc1.b3754036;view=1up;seq=141 p.B39]]
 * @see Astronomy and Astrophysics 134 (1984) [[http://articles.adsabs.harvard.edu/cgi-bin/nph-iarticle_query?bibcode=1984A%26A...134....1L&db_key=AST&page_ind=0&data_type=GIF&type=SCREEN_VIEW&classic=YES p.1-6]]
 * @param baseCoordinates observed coordinates at `epoch`
 * @param epoch           time of the base observation; typically `Epoch.J2000`
 * @param properVelocity  proper velocity '''per year''' in [[RightAscension]] and [[Declination]], if any
 * @param radialVelocity  radial velocity (km/y, positive if receding), if any
 * @param parallax        parallax, if any
 */
final case class ProperMotion(
  baseCoordinates: Coordinates,
  epoch:           Epoch,
  properVelocity:  Option[Offset],
  radialVelocity:  Option[RadialVelocity],
  parallax:        Option[Angle]
) {

  def at(i: Instant): ProperMotion =
    plusYears(epoch.untilInstant(i))

  /** Coordinates `elapsedYears` fractional epoch-years after `epoch`. */
  def plusYears(elapsedYears: Double): ProperMotion =
    ProperMotion(
      ProperMotion.properMotion(
        baseCoordinates,
        epoch,
        properVelocity.orEmpty,
        radialVelocity.getOrElse(RadialVelocity.Zero).toKilometersPerSecond,
        parallax.orEmpty,
        elapsedYears
      ),
      epoch.plusYears(elapsedYears),
      properVelocity,
      radialVelocity,
      parallax
    )

}


object ProperMotion {
  import PhysicalConstants.{ AstronomicalUnit, TwoPi }

  def const(cs: Coordinates): ProperMotion =
    ProperMotion(cs, Epoch.J2000, None, None, None)

  /**
   * Proper motion correction in model units.
   * @param baseCoordinates base coordinates
   * @param epoch           the epoch
   * @param properVelocity  proper velocity per epoch year
   * @param radialVelocity  radial velocity (km/sec, positive if receding)
   * @param parallax        parallax
   * @param elapsedYears    elapsed time in epoch years
   * @return Coordinates corrected for proper motion
   */
  def properMotion(
    baseCoordinates: Coordinates,
    epoch:           Epoch,
    properVelocity:  Offset,
    radialVelocity:  Double,
    parallax:        Angle,
    elapsedYears:    Double
  ): Coordinates = {
    val (ra, dec) = properMotionʹ(
      baseCoordinates.toRadians,
      epoch.scheme.lengthOfYear,
      properVelocity.toRadians,
      radialVelocity,
      parallax.toMicroarcseconds.toDouble / 1000000.0,
      elapsedYears
    )
    Coordinates.unsafeFromRadians(ra, dec)
  }

  // Some constants we need
  private val secsPerDay  = 86400.0
  private val auPerKm     = 1000.0 / AstronomicalUnit.toDouble
  private val radsPerAsec = Angle.fromArcseconds(1).toDoubleRadians

  // We need to do things with little vectors of doubles
  private type Vec2 = (Double, Double)
  private type Vec3 = (Double, Double, Double)

  // |+| gives us addition for VecN, but we also need scalar multiplication
  private implicit class Vec3Ops(a: Vec3) {
    def *(d: Double): Vec3 =
      (a._1 * d, a._2 * d, a._3 * d)
  }

  /**
   * Proper motion correction in base units.
   * @param baseCoordinates base (ra, dec) in radians, [0 … 2π) and (-π/2 … π/2)
   * @param daysPerYear     length of epoch year in fractonal days
   * @param properVelocity  proper velocity in (ra, dec) in radians per epoch year
   * @param radialVelocity  radial velocity (km/sec, positive means away from earth)
   * @param parallax        parallax in arcseconds (!)
   * @param elapsedYears    elapsed time in epoch years
   * @return (ra, dec) in radians, corrected for proper motion
   */
  private def properMotionʹ(
    baseCoordinates: Vec2,
    daysPerYear:     Double,
    properVelocity:  Vec2,
    parallax:        Double,
    radialVelocity:  Double,
    elapsedYears:    Double
  ): Vec2 = {

    // Break out our components
    val (ra,   dec) = baseCoordinates
    val (dRa, dDec) = properVelocity

    // Convert to cartesian
    val pos: Vec3 = {
      val cd = cos(dec)
      (cos(ra) * cd, sin(ra) * cd, sin(dec))
    }

    // Change per year due to radial velocity and parallax. The units work out to asec/y.
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

    // Our new position (still in polar coordinates). `|+|` here is scalar addition provided by
    // cats … unlike scalaz it does give you Semigroup[Double] even though it's not strictly lawful.
    val pʹ = pos |+| ((dPos1 |+| dPos2) * elapsedYears)

    // Back to spherical
    val (x, y, z) = pʹ
    val r    = hypot(x, y)
    val raʹ  = if (r === 0.0) 0.0 else atan2(y, x)
    val decʹ = if (z === 0.0) 0.0 else atan2(z, r)
    val raʹʹ = {
      // Normalize to [0 .. 2π)
      val rem = raʹ % TwoPi
      if (rem < 0.0) rem + TwoPi else rem
    }
    (raʹʹ, decʹ)

  }

}


object ProperMotionExample {

  /** Proper motion of Barnard's Star, for reference. */
  val Barnard: ProperMotion =
    ProperMotion(
      Coordinates.parse("17 57 48.49803 +04 41 36.2072").getOrElse(sys.error("oops")),
      Epoch.J2000,
      Some(Offset(
        Offset.P(Angle.fromMicroarcseconds( -798580L)),
        Offset.Q(Angle.fromMicroarcseconds(10328120L))
      )),
      Some(RadialVelocity.fromKilometersPerSecond(-110.6)),
      Some(Angle.fromMicroarcseconds(545620L))
    )

  def main(args: Array[String]): Unit = {
    (0.0 to 10.0 by 1.0) foreach { y =>
      val cs = Barnard.plusYears(y).baseCoordinates
      println(s"${2000 + y} -> $cs")
    }
  }

}
