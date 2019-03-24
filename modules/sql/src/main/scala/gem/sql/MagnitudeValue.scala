// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql

import java.math.RoundingMode.HALF_UP

/** Minimal MagnitudeValue class to support reading from the enum
  * tables.
  */
final case class MagnitudeValue(value: BigDecimal) extends Product with Serializable {
  def toScaledInt: Int = value.underlying.scaleByPowerOfTen(2).setScale(0, HALF_UP).intValue
}
