// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Order, Show }
import cats.implicits._
import gem.math.PhysicalConstants.SpeedOfLight
import scala.math.sqrt

/**
 * Radial Velocity represented as integral meters per second, positive if receding. We can also
 * view velocity in terms of redshift (a dimensionless value called '''z''').
 * @see Wikipedia on [[https://en.wikipedia.org/wiki/Radial_velocity Radial Velocity]]
 * @see Wikipedia on [[https://en.wikipedia.org/wiki/Redshift Redshift]]
 */
final case class RadialVelocity(toMetersPerSecond: Int) {

  // Sanity check
  assert(toMetersPerSecond.abs <= SpeedOfLight, "Radial velocity exceeds the speed of light.")

  def toKilometersPerSecond: Double =
    toMetersPerSecond.toDouble / 1000.0

  def toRedshift: Double = {
    val v = toMetersPerSecond.toDouble
    val C = SpeedOfLight.toDouble
    val t = (1.0 + v / C) / (1.0 - v / C)
    sqrt(t) - 1.0
  }

  override def toString =
    f"RadialVelocity($toKilometersPerSecond%3.3fkm/s)"

}

object RadialVelocity {

  /** Radial velocity of zero. */
  val Zero: RadialVelocity = RadialVelocity(0)

  /** Construct a [[RadialVelocity]] from floating point kilometers per second. */
  def fromKilometersPerSecond(kms: Double): RadialVelocity =
    RadialVelocity((kms * 1000.0).toInt)

  def fromRedshift(z: Double): RadialVelocity = {
    val C = SpeedOfLight.toDouble
    val m = C * ((z + 1.0) * (z + 1.0) - 1.0) / ((z + 1.0) * (z + 1.0) + 1.0)
    RadialVelocity(m.toInt)
  }

  /** Instances are ordered by their `.toMetersPerSecond` values. */
  implicit val RadialVelocityOrder: Order[RadialVelocity] =
    Order.by(_.toMetersPerSecond)

  implicit val RadialVelocityShow: Show[RadialVelocity] =
    Show.fromToString

}
