// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.math.{ Angle, Ephemeris, Offset }

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
  def velocityCompression[F[_]](µas: Long): Pipe[F, Ephemeris.Element, Ephemeris.Element] = {
    def µasDouble(e: Ephemeris.Element)(f: Offset => Angle): Double =
      f(e._2.delta).toSignedMicroarcseconds.toDouble

    def µasP(e: Ephemeris.Element) = µasDouble(e)(_.p.toAngle)
    def µasQ(e: Ephemeris.Element) = µasDouble(e)(_.q.toAngle)

    def ΔVelocity(e0: Ephemeris.Element, e1: Ephemeris.Element): Long = {
      val dp = µasP(e0) - µasP(e1)
      val dq = µasQ(e0) - µasQ(e1)
      Math.sqrt(dp * dp + dq * dq).round
    }

    _.zipWithNext
     .filterWithPrevious { case ((prev, _), (cur, onext)) =>
       onext.forall { _ => ΔVelocity(prev, cur) >= µas }
     }.map(_._1)
  }

  /** An `fs2.Pipe` that passes only elements which differ by more than the
    * standard delta velocity.
    */
  def standardVelocityCompression[F[_]]: Pipe[F, Ephemeris.Element, Ephemeris.Element] =
    velocityCompression(StandardVelocityLimitµas)
}

object VelocityCompression extends VelocityCompression
