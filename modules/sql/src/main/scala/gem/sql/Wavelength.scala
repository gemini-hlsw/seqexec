// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql

import java.math.RoundingMode.HALF_UP

/** Minimal Wavelength classes to support reading from the enum
  * tables.
  */
sealed trait Wavelength extends Product with Serializable {
  def toBigDecimal: BigDecimal

  def toPicometers: Int = {
    val n = this match {
      case Wavelength.Pm(_) => 0
      case Wavelength.Nm(_) => 3
      case Wavelength.Um(_) => 6
    }
    toBigDecimal.underlying.scaleByPowerOfTen(n).setScale(0, HALF_UP).intValue
  }

}

object Wavelength {
  final case class Pm(toBigDecimal: BigDecimal) extends Wavelength
  final case class Nm(toBigDecimal: BigDecimal) extends Wavelength
  final case class Um(toBigDecimal: BigDecimal) extends Wavelength

  def fromPm(bd: BigDecimal): Wavelength.Pm =
    Pm(bd)

  def fromNm(bd: BigDecimal): Wavelength.Nm =
    Nm(bd)

  def fromUm(bd: BigDecimal): Wavelength.Um =
    Um(bd)
}
