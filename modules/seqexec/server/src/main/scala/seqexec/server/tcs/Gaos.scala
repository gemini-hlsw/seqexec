// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.{Eq, Show}
import cats.implicits._
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.tcs.Gaos.PauseCondition.{FixedPauseCondition, OffsetMove}
import seqexec.server.tcs.Gaos.ResumeCondition.{FixedResumeCondition, OffsetReached}
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

  sealed trait PauseCondition extends Product with Serializable

  object PauseCondition {
    // Telescope offset will be changed
    final case class OffsetMove(newOffset: FocalPlaneOffset) extends PauseCondition

    sealed trait FixedPauseCondition extends PauseCondition

    // OI will be turn off
    case object OiOff extends FixedPauseCondition
    // PI will be turn off
    case object P1Off extends FixedPauseCondition
    // Unguided step
    case object GaosGuideOff extends FixedPauseCondition
    // Instrument config (affects ODGW)
    case object InstConfigMove extends FixedPauseCondition

    implicit val offsetMoveEq: Eq[OffsetMove] = Eq.by(_.newOffset)

    implicit val fixedPauseReasonEq: Eq[FixedPauseCondition] = Eq.fromUniversalEquals

    implicit val pauseReasonEq: Eq[PauseCondition] = Eq.instance{
      case (a: OffsetMove, b: OffsetMove)                   => a === b
      case (a: FixedPauseCondition, b: FixedPauseCondition) => a === b
      case _                                                => false
    }

  }

  // Non-repeatable collection of PauseCondition that admits only one OffsetMove
  final case class PauseConditionSet private (offsetO: Option[OffsetMove], fixed: Set[FixedPauseCondition]){

    def +(v: PauseCondition): PauseConditionSet = v match {
      case a: OffsetMove          => PauseConditionSet(a.some, fixed)
      case b: FixedPauseCondition => PauseConditionSet(offsetO, fixed + b)
    }

    def contains(v: FixedPauseCondition): Boolean = fixed.contains(v)

  }

  object PauseConditionSet{

    def fromList(ss: List[PauseCondition]): PauseConditionSet = ss.foldLeft(empty){case (xx, e) => xx + e}

    val empty: PauseConditionSet = PauseConditionSet(None, Set.empty)

    implicit val pauseConditionSetEq: Eq[PauseConditionSet] = Eq.by{x => (x.offsetO, x.fixed)}

    implicit val pauseConditionSetShow: Show[PauseConditionSet] = Show.show(
      x => (x.offsetO.toList ++ x.fixed.toList).mkString
    )

  }

  sealed trait ResumeCondition extends Product with Serializable

  object ResumeCondition {
    // Telescope offset will be changed
    final case class OffsetReached(newOffset: FocalPlaneOffset) extends ResumeCondition

    sealed trait FixedResumeCondition extends ResumeCondition

    // OI will be turn off
    case object OiOn extends FixedResumeCondition
    // PI will be turn off
    case object P1On extends FixedResumeCondition
    // Guided step
    case object GaosGuideOn extends FixedResumeCondition
    // Instrument config (affects ODGW)
    case object InstConfigCompleted extends FixedResumeCondition

    implicit val offsetReachedEq: Eq[OffsetReached] = Eq.by(_.newOffset)

    implicit val fixedResumeReasonEq: Eq[FixedResumeCondition] = Eq.instance {
      case (OiOn, OiOn)                               => true
      case (P1On, P1On)                               => true
      case (GaosGuideOn, GaosGuideOn)                 => true
      case (InstConfigCompleted, InstConfigCompleted) => true
      case _                                          => false
    }

    implicit val resumeReasonEq: Eq[ResumeCondition] = Eq.instance {
      case (a: OffsetReached, b: OffsetReached)               => a === b
      case (a: FixedResumeCondition, b: FixedResumeCondition) => a === b
      case _                                                  => false
    }
  }

  // Non-repeatable collection of ResumeCondition that admits only one OffsetMove
  final case class ResumeConditionSet private (offsetO: Option[OffsetReached], fixed: Set[FixedResumeCondition]){

    def +(v: ResumeCondition): ResumeConditionSet = v match {
      case a: OffsetReached        => ResumeConditionSet(a.some, fixed)
      case b: FixedResumeCondition => ResumeConditionSet(offsetO, fixed + b)
    }

    def contains(v: FixedResumeCondition): Boolean = fixed.contains(v)

  }

  object ResumeConditionSet{

    def fromList(ss: List[ResumeCondition]): ResumeConditionSet = ss.foldLeft(empty){case (xx, e) => xx + e}

    val empty: ResumeConditionSet = ResumeConditionSet(None, Set.empty)

    implicit val resumeConditionSetEq: Eq[ResumeConditionSet] = Eq.by{x => (x.offsetO, x.fixed)}

    implicit val resumeConditionSetShow: Show[ResumeConditionSet] = Show.show(
      x => (x.offsetO.toList ++ x.fixed.toList).mkString
    )

  }

  sealed case class PauseResume[F[_]](
    pause: Option[F[Unit]], // None means Gaos will not be paused
    resume: Option[F[Unit]] // None means Gaos will not be resumed
  )

}
