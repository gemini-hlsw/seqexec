// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql

/** A very minimal Angle class to support reading from the enum tables without depending on core. */
final case class Angle(toArcsecs: Double)
object Angle {
  def fromArcsecs(d: Double): Angle =
    Angle(d)
}
