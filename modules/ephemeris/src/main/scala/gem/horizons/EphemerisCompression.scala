// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.math.Ephemeris
import gsp.math.{ Angle, Offset }
import gem.syntax.stream._

import cats.implicits._

import fs2.Pipe

import java.time.Duration


/** FS2 pipes for discarding ephemeris elements that differ one from the other
  * by less than a given Δ velocity or accumulated acceleration.
  */
trait EphemerisCompression {

  import EphemerisCompression._

  /** Standard velocity threshold for compression, 0.1 arcsec/hour. */
  val StandardVelocityLimitµas: Long =
    Angle.milliarcseconds.reverseGet(100).toMicroarcseconds

  /** An `fs2.Pipe` that passes only elements which differ by more than the
    * provided delta velocity.
    *
    * @param µas difference in velocity below this threshold result in
    *            skipping elements
    */
  def velocityCompression[F[_]](µas: Long): Pipe[F, Ephemeris.Element, Ephemeris.Element] = {
    def Δv(e0: Element, e1: Element): Long = {
      val dp = e0.µasP - e1.µasP
      val dq = e0.µasQ - e1.µasQ
      math.hypot(dp, dq).round
    }

    _.zipWithNext
     .filterWithPrevious { case ((prev, _), (cur, onext)) =>
       onext.forall { _ => Δv(prev, cur) > µas }
     }.map(_._1)
  }

  /** An `fs2.Pipe` that passes only elements which differ by more than the
    * standard delta velocity.
    */
  def standardVelocityCompression[F[_]]: Pipe[F, Ephemeris.Element, Ephemeris.Element] =
    velocityCompression(StandardVelocityLimitµas)


  /** Standard acceleration threshold for compression, 0.2 arcsec/hour^2. */
  val StandardAccelerationLimitµas: Long =
    Angle.milliarcseconds.reverseGet(200).toMicroarcseconds

  /** An `fs2.Pipe` that passes only elements which sufficiently differ by an
    * accumulated average acceleration.
    *
    * @param µas difference in acceleration below this threshold result in
    *            skipping elements
    */
  def accelerationCompression[F[_]](µas: Long): Pipe[F, Element, Element] = {

    final case class SumAvgs(a: AvgAcceleration, n: Int) {
      def +(next: AvgAcceleration): SumAvgs =
        SumAvgs(a + next, n + 1)

      def keep: Boolean =
        (0.5 * a.acceleration / n.toDouble * a.hours * a.hours).round > µas
    }

    val Zero = Option(SumAvgs(AvgAcceleration.Zero, 0))

    // The fold function.  It is complicated by the use of Option to ensure that
    // the first and last element are always included. The `oa` parameter below
    // is `None` for the first and last element, which means the sum including
    // `oa` is `None`, which the filter function `_.forall(_.keep)` passes.
    def f(os: Option[SumAvgs], oa: Option[AvgAcceleration]): Option[SumAvgs] =
      (os, oa).tupled.map { case (sum, a) => sum + a }

    _.zipWithPreviousAndNext
     .map {
       case (None,       cur, _   ) => (Option.empty[AvgAcceleration],            cur) // First element (should always be included)
       case (_,          cur, None) => (Option.empty[AvgAcceleration],            cur) // Last element (should always be included)
       case (Some(prev), cur, _   ) => (Some(AvgAcceleration.between(prev, cur)), cur)
    }.filterOnFold(Zero)((os, oe) => f(os, oe._1), _.forall(_.keep))
     .map(_._2) // forget the avg acceleration and just extract the element
  }

  /** An `fs2.Pipe` that passes only elements which differ by more than the
    * standard average acceleration.
    */
  def standardAccelerationCompression[F[_]]: Pipe[F, Ephemeris.Element, Ephemeris.Element] =
    accelerationCompression(StandardAccelerationLimitµas)
}

object EphemerisCompression extends EphemerisCompression {

  private type Element = Ephemeris.Element

  private class ElementOps(val self: Element) extends AnyVal {
    private def µas(f: Offset => Angle): Double =
      Angle.signedMicroarcseconds.get(f(self._2.delta)).toDouble

    // Extracts the velocity in p as decimal microarcseconds / hour
    def µasP: Double =
      µas(_.p.toAngle)

    // Extracts the velocity in q as decimal microarcseconds / hour
    def µasQ: Double =
      µas(_.q.toAngle)
  }

  private implicit def ToElementOps(e: Element): ElementOps = new ElementOps(e)

  /** Average acceleration and the time amount over which it is averaged.
    *
    * @param acceleration microarcseconds / hour / hour
    * @param hours        decimal hours
    */
  private final case class AvgAcceleration(acceleration: Double, hours: Double) {

    def +(that: AvgAcceleration): AvgAcceleration =
      AvgAcceleration(acceleration + that.acceleration, hours + that.hours)
  }

  private object AvgAcceleration {

    val Zero = AvgAcceleration(0.0, 0.0)

    /** Calculates the average acceleration from one element to the next. */
    def between(prev: Element, cur: Element): AvgAcceleration = {
      // time since previous
      val d = Duration.between(prev._1.toInstant, cur._1.toInstant)

      // time since previous as decimal hours
      val h = d.toMillis.toDouble / Duration.ofHours(1).toMillis.toDouble

      val dp = (cur.µasP - prev.µasP)/h
      val dq = (cur.µasQ - prev.µasQ)/h

      AvgAcceleration(math.hypot(dp, dq), h)
    }
  }

}
