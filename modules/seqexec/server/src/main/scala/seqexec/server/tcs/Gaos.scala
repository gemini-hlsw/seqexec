// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.Eq
import cats.implicits._
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.tcs.TcsController.FocalPlaneOffset
import squants.Time

/*
 * Interface to control AO systems. Implemented by Altair and GeMS
 */
trait Gaos[F[_]] {

  /*
   * Notify GAOS system of the start of the observation
   */
  def observe(config: Either[AltairConfig, GemsConfig], expTime: Time): F[Unit]

  /*
   * Notify GAOS system of the end of the observation
   */
  def endObserve(config: Either[AltairConfig, GemsConfig]): F[Unit]

}

object Gaos {
  sealed trait PauseCondition

  // Telescope offset will be changed
  final case class OffsetMove(previousOffset: FocalPlaneOffset, newOffset: FocalPlaneOffset) extends PauseCondition
  // OI will be turn off
  case object OiOff extends PauseCondition
  // PI will be turn off
  case object P1Off extends PauseCondition
  // Unguided step
  case object GaosGuideOff extends PauseCondition

  sealed trait ResumeCondition
  // Telescope offset will be changed
  final case class OffsetReached(newOffset: FocalPlaneOffset) extends ResumeCondition
  // OI will be turn off
  case object OiOn extends ResumeCondition
  // PI will be turn off
  case object P1On extends ResumeCondition
  // Guided step
  case object GaosGuideOn extends ResumeCondition

  implicit val becauseOffsetMoveEq: Eq[OffsetMove] = Eq.by(x => (x.previousOffset, x.newOffset))

  implicit val pauseReasonEq: Eq[PauseCondition] = Eq.instance{
    case (a: OffsetMove, b: OffsetMove) => a === b
    case (OiOff, OiOff)                 => true
    case (P1Off, P1Off)                 => true
    case (GaosGuideOff, GaosGuideOff)   => true
    case _                              => false
  }

  implicit val becauseOffsetReachedEq: Eq[OffsetReached] = Eq.by(_.newOffset)

  implicit val resumeReasonEq: Eq[ResumeCondition] = Eq.instance{
    case (a: OffsetReached, b: OffsetReached) => a === b
    case (OiOn, OiOn)                         => true
    case (P1On, P1On)                         => true
    case (GaosGuideOn, GaosGuideOn)           => true
    case _                                    => false
  }

  sealed case class PauseResume[F[_]](
    pause: Option[F[Unit]], // None means Gaos will not be paused
    resume: Option[F[Unit]] // None means Gaos will not be resumed
  )

}
