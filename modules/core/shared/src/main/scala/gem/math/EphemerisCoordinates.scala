// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gsp.math._

import cats._
import monocle.Lens
import monocle.macros.GenLens

/** A coordinate along with a rate of change in RA and Dec for some time unit,
  * expressed as an offset in p and q.  In reality the velocity information
  * comes from horizons and is always arcseconds per hour in horizons data.
  *
  * @param coord coordinates
  * @param delta rate of change in RA and dec, where the delta(RA)/time has been
  *              multiplied by the cosine of the dec
  */
final case class EphemerisCoordinates(
                   coord: Coordinates,
                   delta: Offset /* per time unit */) {

  /** Interpolates the position and rate of change at a point between this
    * coordinate and the given coordinate.
    */
  def interpolate(that: EphemerisCoordinates, f: Double): EphemerisCoordinates = {
    def interpolateAngle(a: Angle, b: Angle): Angle =
      Angle.fromMicroarcseconds(
        (Angle.signedMicroarcseconds.get(a).toDouble * (1 - f) + Angle.signedMicroarcseconds.get(b) * f).round
      )

    val coordʹ = coord.interpolate(that.coord, f)
    val pʹ     = interpolateAngle(delta.p.toAngle, that.delta.p.toAngle)
    val qʹ     = interpolateAngle(delta.q.toAngle, that.delta.q.toAngle)

    EphemerisCoordinates(coordʹ, Offset(Offset.P(pʹ), Offset.Q(qʹ)))
  }

}

object EphemerisCoordinates extends EphemerisCoordinatesOptics {

  val Zero: EphemerisCoordinates =
    EphemerisCoordinates(Coordinates.Zero, Offset.Zero)

  /** @group Typeclass Instances */
  implicit val EphemerisCoordinatesEqual: Eq[EphemerisCoordinates] =
    Eq.fromUniversalEquals

  /** @group Typeclass Instances */
  implicit val ShowEphemerisCoordinates: Show[EphemerisCoordinates] =
    Show.fromToString

}

trait EphemerisCoordinatesOptics {

  /** @group Optics */
  val coordinates: Lens[EphemerisCoordinates, Coordinates] =
    GenLens[EphemerisCoordinates](_.coord)

  /** @group Optics */
  val delta: Lens[EphemerisCoordinates, Offset] =
    GenLens[EphemerisCoordinates](_.delta)

  /** @group Optics */
  val rightAscension: Lens[EphemerisCoordinates, RightAscension] =
    coordinates composeLens Coordinates.rightAscension

  /** @group Optics */
  val declination: Lens[EphemerisCoordinates, Declination] =
    coordinates composeLens Coordinates.declination

  /** @group Optics */
  val deltaP: Lens[EphemerisCoordinates, Offset.P] =
    delta composeLens Offset.p

  /** @group Optics */
  val deltaQ: Lens[EphemerisCoordinates, Offset.Q] =
    delta composeLens Offset.q

}