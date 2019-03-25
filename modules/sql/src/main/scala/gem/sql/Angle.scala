// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql

/** Minimal Angle classes to support reading from the enum tables without depending on core. */
object Angle {

  final case class Arcseconds(toArcsecs: BigDecimal)
  object Arcseconds {
    def fromArcsecs(d: BigDecimal): Arcseconds =
      Arcseconds(d)
  }

  final case class Degrees(toDegrees: BigDecimal)
  object Degrees {
    def fromDegrees(d: BigDecimal): Degrees =
      Degrees(d)
  }

}
