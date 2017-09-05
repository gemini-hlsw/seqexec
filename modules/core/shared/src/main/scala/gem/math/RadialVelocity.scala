// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Order, Show }
import cats.implicits._
import gem.math.PhysicalConstants.SpeedOfLight

/** Radial Velocity represented as integral meters per second, positive if receding. */
final case class RadialVelocity(toMetersPerSecond: Int) {

  // Sanity check
  assert(toMetersPerSecond.abs <= SpeedOfLight, "Radial velocity exceeds the speed of light.")

  def toKilometersPerSecond: Double =
    toMetersPerSecond.toDouble / 1000

  override def toString =
    f"RadialVelocity($toKilometersPerSecond%3.3fkm/s)"

}

object RadialVelocity {

  /** Radial velocity of zero. */
  val Zero: RadialVelocity = RadialVelocity(0)

  /** Construct a [[RadialVelocity]] from floating point kilometers per second. */
  def fromKilometersPerSecond(kms: Double): RadialVelocity =
    RadialVelocity((kms * 1000.0).toInt)

  /** Instances are ordered by their `.toMetersPerSecond` values. */
  implicit val RadialVelocityOrder: Order[RadialVelocity] =
    Order.by(_.toMetersPerSecond)

  implicit val RadialVelocityShow: Show[RadialVelocity] =
    Show.fromToString

}
