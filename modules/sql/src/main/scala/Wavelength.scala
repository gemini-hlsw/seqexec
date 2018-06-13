// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql

import java.math.RoundingMode.HALF_UP

/** Minimal Wavelength classes to support reading from the enum
  * tables.
  */
sealed trait Wavelength extends Product with Serializable {
  def toBigDecimal: BigDecimal

  def toAngstrom: Int = {
    val n = this match {
      case Wavelength.Nm(_) => 1
      case Wavelength.Um(_) => 4
    }
    toBigDecimal.underlying.scaleByPowerOfTen(n).setScale(0, HALF_UP).intValue
  }
}

object Wavelength {
  final case class Nm(toBigDecimal: BigDecimal) extends Wavelength
  final case class Um(toBigDecimal: BigDecimal) extends Wavelength

  def fromNm(bd: BigDecimal): Wavelength.Nm =
    Nm(bd)

  def fromUm(bd: BigDecimal): Wavelength.Um =
    Um(bd)
}
