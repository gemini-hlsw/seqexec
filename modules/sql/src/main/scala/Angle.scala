// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql

/** Minimal Angle classes to support reading from the enum tables without depending on core. */
object Angle {

  final case class Arcseconds(toArcsecs: Double)
  object Arcseconds {
    def fromArcsecs(d: Double): Arcseconds =
      Arcseconds(d)
  }

  final case class Degrees(toDegrees: Double)
  object Degrees {
    def fromDegrees(d: Double): Degrees =
      Degrees(d)
  }

}
