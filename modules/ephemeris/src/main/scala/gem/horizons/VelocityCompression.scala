// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.math.{ Angle, Ephemeris }

import fs2.Pipe


/** FS2 pipes for discarding ephemeris elements that differ one from the other
  * by less than a given Δ velocity.
  */
trait VelocityCompression {

  /** Standard velocity threshold for compression, 0.1 arcsec/hour. */
  val StandardVelocityLimitµas: Long =
    Angle.fromMilliarcseconds(100).toMicroarcseconds

  /** An `fs2.Pipe` that passes only elements which differ by more than the
    * provided delta velocity.
    *
    * @param µas difference in velocity below this threshold result in
    *            skipping elements
    */
  def velocityCompression[F[_]](µas: Long): Pipe[F, Ephemeris.Element, Ephemeris.Element] =
    _.zipWithNext
     .map { case (e, on) => (e, e._2.velocity, on.isEmpty) }
     .filterWithPrevious { case ((_, pv, _), (_, v, last)) =>
       // always include the last element regardless of Δ velocity
       last || (pv.toMicroarcseconds - v.toMicroarcseconds).abs >= µas
     }.map(_._1)

  /** An `fs2.Pipe` that passes only elements which differ by more than the
    * standard delta velocity.
    */
  def standardVelocityCompression[F[_]]: Pipe[F, Ephemeris.Element, Ephemeris.Element] =
    velocityCompression(StandardVelocityLimitµas)
}

object VelocityCompression extends VelocityCompression
