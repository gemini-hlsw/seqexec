// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

/** A coordinate along with a rate of change in RA and Dec for some time unit,
  * expressed as an offset in p and q.  In reality the velocity information
  * comes from horizons and is always arcseconds per hour.
  */
final case class EphemerisCoordinates(
                   coord: Coordinates,
                   velocity: Offset   /* per hour */) {

  def interpolate(that: EphemerisCoordinates, f: Double): EphemerisCoordinates = {
    def interpolateAngle(a: Angle, b: Angle): Angle =
      Angle.fromMicroarcseconds(
        (a.toMicroarcseconds.toDouble * (1 - f) + b.toMicroarcseconds * f).round
      )

    val coordʹ = coord.interpolate(that.coord, f)
    val pʹ     = interpolateAngle(velocity.p.toAngle, that.velocity.p.toAngle)
    val qʹ     = interpolateAngle(velocity.q.toAngle, that.velocity.q.toAngle)

    EphemerisCoordinates(coordʹ, Offset(Offset.P(pʹ), Offset.Q(qʹ)))
  }
}

