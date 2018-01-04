// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Order, Show }
import cats.instances.int._

/**
 * Exact magnitude value represented as unsigned integral
 * @param value This magnitude integral value
 */
final case class MagnitudeValue(val value: Int)

object MagnitudeValue {

  final lazy val ZeroMagnitude = MagnitudeValue(0)

  implicit val MagnitudeValueShow: Show[MagnitudeValue] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val MagnitudeValueOrder: Order[MagnitudeValue] =
    Order.by(_.value)

}
