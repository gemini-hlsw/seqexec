// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql

import java.math.RoundingMode.HALF_UP

/** Minimal time classes to support reading from the enum
  * tables.
  */
sealed trait FiniteDuration extends Product with Serializable {
    def toBigDecimal: BigDecimal

  def toMillis: Long = {
    val n = this match {
      case FiniteDuration.Milliseconds(_) => 0
      case FiniteDuration.Seconds(_)      => 3
    }
    toBigDecimal.underlying.scaleByPowerOfTen(n).setScale(0, HALF_UP).longValue
  }

}

object FiniteDuration {
  final case class Seconds(toBigDecimal: BigDecimal) extends FiniteDuration
  final case class Milliseconds(toBigDecimal: BigDecimal) extends FiniteDuration

  def fromSeconds(bd: BigDecimal): FiniteDuration.Seconds =
    Seconds(bd)

  def fromMilliseconds(bd: BigDecimal): FiniteDuration.Milliseconds =
    Milliseconds(bd)
}
