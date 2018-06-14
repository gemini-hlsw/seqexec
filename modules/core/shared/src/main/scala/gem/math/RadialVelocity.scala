// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Order, Show }
import cats.implicits._
import gem.math.PhysicalConstants.SpeedOfLight
import gem.optics.Format
import gem.syntax.prism._
import scala.math.sqrt
import monocle.Prism

/**
 * Radial Velocity represented as integral meters per second, positive if receding. We can also
 * view velocity in terms of redshift (a dimensionless value called '''z''').
 * @see Wikipedia on [[https://en.wikipedia.org/wiki/Radial_velocity Radial Velocity]]
 * @see Wikipedia on [[https://en.wikipedia.org/wiki/Redshift Redshift]]
 */
sealed abstract case class RadialVelocity(toMetersPerSecond: Int) {

  // Sanity check
  assert(toMetersPerSecond.abs <= SpeedOfLight, "Radial velocity exceeds the speed of light.")

  def toDoubleKilometersPerSecond: Double =
    toMetersPerSecond.toDouble / 1000.0

  def toRedshift: Double = {
    val v = toMetersPerSecond.toDouble
    val C = SpeedOfLight.toDouble
    val t = (1.0 + v / C) / (1.0 - v / C)
    sqrt(t) - 1.0
  }

  override def toString =
    f"RadialVelocity($toDoubleKilometersPerSecond%3.3fkm/s)"

}

object RadialVelocity {

  val fromMetersPerSecond: Prism[Int, RadialVelocity] =
    Prism(Some(_: Int).filter(_.abs <= SpeedOfLight).map(new RadialVelocity(_) {}))(_.toMetersPerSecond)

  val fromKilometersPerSecond: Format[BigDecimal, RadialVelocity] =
    Format.fromPrism(fromMetersPerSecond)
      .imapA(
        n => new java.math.BigDecimal(n).movePointLeft(3),
        d => d.underlying.movePointRight(3).intValue
      )

  /** Radial velocity of zero. */
  val Zero: RadialVelocity = fromMetersPerSecond.unsafeGet(0)

  /** Construct a [[RadialVelocity]] from floating point kilometers per second. */
  def unsafeFromDoubleKilometersPerSecond(kms: Double): RadialVelocity =
    fromKilometersPerSecond.unsafeGet(BigDecimal(kms))

  def unsafeFromRedshift(z: Double): RadialVelocity = {
    val C = SpeedOfLight.toDouble
    val m = C * ((z + 1.0) * (z + 1.0) - 1.0) / ((z + 1.0) * (z + 1.0) + 1.0)
    fromMetersPerSecond.unsafeGet(m.toInt)
  }

  /** Instances are ordered by their `.toMetersPerSecond` values. */
  implicit val RadialVelocityOrder: Order[RadialVelocity] =
    Order.by(_.toMetersPerSecond)

  implicit val RadialVelocityShow: Show[RadialVelocity] =
    Show.fromToString

}
