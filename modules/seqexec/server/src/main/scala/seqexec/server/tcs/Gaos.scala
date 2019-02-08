// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.Eq
import cats.implicits._
import seqexec.server.tcs.TcsController.InstrumentOffset
import seqexec.server.tcs.TcsController.InstrumentOffset._
import squants.Time

/*
 * Interface to control AO systems. Implemented by Altair and GeMS
 */
trait Gaos[F[_]] {
  import Gaos._

  /*
   * Pause GAOS guiding. The GAOS system decides what to pause, according to the reasons given
   */
  def pause(reasons: Set[PauseReason]): F[Unit]
  /*
   * Resume GAOS guiding. The GAOS system decides what to resume, according to the reasons given
   *
   */
  def resume(reasons: Set[ResumeReason]): F[Unit]

  /*
   * Notify GAOS system of the start of the observation
   */
  def observe(expTime: Time): F[Unit]
  /*
   * Notify GAOS system of the end of the observation
   */
  def endObserve: F[Unit]

}

object Gaos {
  sealed trait PauseReason

  // Telescope offset will be changed
  final case class BecauseOffsetMove(previousOffset: InstrumentOffset,
                                       newOffset: InstrumentOffset)
    extends PauseReason
  // OI will be turn off
  case object BecauseOiOff extends PauseReason
  // PI will be turn off
  case object BecauseP1Off extends PauseReason
  // GAOS star guide will be turn off
  case object BecauseGaosStarOff extends PauseReason

  sealed trait ResumeReason
  // Telescope offset will be changed
  final case class BecauseOffsetReached(newOffset: InstrumentOffset) extends ResumeReason
  // OI will be turn off
  case object BecauseOiOn extends ResumeReason
  // PI will be turn off
  case object BecauseP1On extends ResumeReason
  // GAOS star guide will be turn off
  case object BecauseGaosStarOn extends ResumeReason

  implicit val becauseOffsetMoveEq: Eq[BecauseOffsetMove] = Eq.by(x =>
    (x.previousOffset, x.newOffset)
  )

  implicit val pauseReasonEq: Eq[PauseReason] = Eq.instance[PauseReason]{
    case (a: BecauseOffsetMove, b: BecauseOffsetMove) => a === b
    case (BecauseOiOff, BecauseOiOff)                 => true
    case (BecauseP1Off, BecauseP1Off)                 => true
    case (BecauseGaosStarOff, BecauseGaosStarOff)     => true
    case _                                            => false
  }

  implicit val becauseOffsetReachedEq: Eq[BecauseOffsetReached] = Eq.by(x =>
    (x.newOffset)
  )

  implicit val resumeReasonEq: Eq[ResumeReason] = Eq.instance[ResumeReason]{
    case (a: BecauseOffsetReached, b: BecauseOffsetReached) => a === b
    case (BecauseOiOn, BecauseOiOn)                         => true
    case (BecauseP1On, BecauseP1On)                         => true
    case (BecauseGaosStarOn, BecauseGaosStarOn)             => true
    case _                                                  => false
  }

}
