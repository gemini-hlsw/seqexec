// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Order, Show }
import cats.instances.int._

/**
 * Exact magnitude value represented as an int with the original value scaled up
 *
 * @param scaledValue This magnitude integral value, as the original multiplied by 100. value is dimensionless
 * @see The Wikipedia [[https://en.wikipedia.org/wiki/Apparent_magnitude]]
 */
final case class MagnitudeValue(private[gem] val scaledValue: Int) extends Product with Serializable {
  def toDoubleValue: Double = scaledValue / 100.0
}

object MagnitudeValue {

  final lazy val ZeroMagnitude = MagnitudeValue(0)

  /** @group Typeclass Instances */
  implicit val MagnitudeValueShow: Show[MagnitudeValue] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val MagnitudeValueOrder: Order[MagnitudeValue] =
    Order.by(_.scaledValue)

}
