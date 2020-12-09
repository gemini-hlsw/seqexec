// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import cats.syntax.all._
import lucuma.core.util.Enumerated
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism
import monocle.macros.Lenses
import seqexec.model.dhs._
import seqexec.model.enum._

sealed trait Step extends Product with Serializable {
  def id: StepId
  def config: StepConfig
  def status: StepState
  def breakpoint: Boolean
  def skip: Boolean
  def fileId: Option[ImageFileId]
}

object Step {
  implicit class LensBooleanOps[A](l: Lens[A, Boolean]) {
    def negate: A => A = l.modify(!_)
  }
  val standardStepP: Prism[Step, StandardStep] =
    GenPrism[Step, StandardStep]

  val nsStepP: Prism[Step, NodAndShuffleStep] =
    GenPrism[Step, NodAndShuffleStep]

  val status: Lens[Step, StepState] =
    Lens[Step, StepState] {
      _.status
    } { n => a =>
      a match {
        case s: StandardStep      => StandardStep.status.set(n)(s)
        case s: NodAndShuffleStep => NodAndShuffleStep.status.set(n)(s)
      }
    }

  val config: Lens[Step, StepConfig] =
    Lens[Step, StepConfig] {
      _.config
    } { n => a =>
      a match {
        case s: StandardStep      => StandardStep.config.set(n)(s)
        case s: NodAndShuffleStep => NodAndShuffleStep.config.set(n)(s)
      }
    }

  val id: Lens[Step, StepId] =
    Lens[Step, StepId] {
      _.id
    } { n => a =>
      a match {
        case s: StandardStep      => StandardStep.id.set(n)(s)
        case s: NodAndShuffleStep => NodAndShuffleStep.id.set(n)(s)
      }
    }

  val skip: Lens[Step, Boolean] =
    Lens[Step, Boolean] {
      _.skip
    } { n => a =>
      a match {
        case s: StandardStep      => StandardStep.skip.set(n)(s)
        case s: NodAndShuffleStep => NodAndShuffleStep.skip.set(n)(s)
      }
    }

  val breakpoint: Lens[Step, Boolean] =
    Lens[Step, Boolean] {
      _.breakpoint
    } { n => a =>
      a match {
        case s: StandardStep      => StandardStep.breakpoint.set(n)(s)
        case s: NodAndShuffleStep => NodAndShuffleStep.breakpoint.set(n)(s)
      }
    }

  val observeStatus: Optional[Step, ActionStatus] =
    Optional[Step, ActionStatus] {
      case s: StandardStep      => s.observeStatus.some
      case s: NodAndShuffleStep => s.nsStatus.observing.some
    } { n => a =>
      a match {
        case s: StandardStep      => StandardStep.observeStatus.set(n)(s)
        case s: NodAndShuffleStep =>
          (NodAndShuffleStep.nsStatus ^|-> NodAndShuffleStatus.observing).set(n)(s)
      }
    }

  val configStatus: Optional[Step, List[(Resource, ActionStatus)]] =
    Optional[Step, List[(Resource, ActionStatus)]] {
      case s: StandardStep      => s.configStatus.some
      case s: NodAndShuffleStep => s.configStatus.some
    } { n => a =>
      a match {
        case s: StandardStep      => StandardStep.configStatus.set(n)(s)
        case s: NodAndShuffleStep => NodAndShuffleStep.configStatus.set(n)(s)
      }
    }

  implicit val equal: Eq[Step] =
    Eq.instance {
      case (x: StandardStep, y: StandardStep)           =>
        x === y
      case (x: NodAndShuffleStep, y: NodAndShuffleStep) =>
        x === y
      case _                                            =>
        false
    }

  implicit class StepOps(val s: Step) extends AnyVal {
    def flipBreakpoint: Step =
      s match {
        case st: StandardStep      => StandardStep.breakpoint.negate(st)
        case st: NodAndShuffleStep => NodAndShuffleStep.breakpoint.negate(st)
        case st                    => st
      }

    def flipSkip: Step =
      s match {
        case st: StandardStep      => StandardStep.skip.negate(st)
        case st: NodAndShuffleStep => NodAndShuffleStep.skip.negate(st)
        case st                    => st
      }

    def file: Option[String] = None

    def canSetBreakpoint(i: Int, firstRunnable: Int): Boolean =
      s.status.canSetBreakpoint(i, firstRunnable)

    def canSetSkipmark: Boolean = s.status.canSetSkipmark

    def hasError: Boolean = s.status.hasError

    def isRunning: Boolean = s.status.isRunning

    def runningOrComplete: Boolean = s.status.runningOrComplete

    def isObserving: Boolean =
      s match {
        case x: StandardStep      => x.observeStatus === ActionStatus.Running
        case x: NodAndShuffleStep => x.nsStatus.observing === ActionStatus.Running
        case _                    => false
      }

    def isObservePaused: Boolean =
      s match {
        case x: StandardStep      => x.observeStatus === ActionStatus.Paused
        case x: NodAndShuffleStep => x.nsStatus.observing === ActionStatus.Paused
        case _                    => false
      }

    def isConfiguring: Boolean =
      s match {
        case x: StandardStep      => x.configStatus.count(_._2 === ActionStatus.Running) > 0
        case x: NodAndShuffleStep => x.configStatus.count(_._2 === ActionStatus.Running) > 0
        case _                    => false
      }

    def isFinished: Boolean = s.status.isFinished

    def wasSkipped: Boolean = s.status.wasSkipped

    def canRunFrom: Boolean = s.status.canRunFrom

    def canConfigure: Boolean = s.status.canConfigure

    def isMultiLevel: Boolean =
      s match {
        case _: NodAndShuffleStep => true
        case _                    => false
      }
  }
}

@Lenses
final case class StandardStep(
  override val id:         StepId,
  override val config:     StepConfig,
  override val status:     StepState,
  override val breakpoint: Boolean,
  override val skip:       Boolean,
  override val fileId:     Option[ImageFileId],
  configStatus:            List[(Resource, ActionStatus)],
  observeStatus:           ActionStatus
) extends Step

object StandardStep {
  implicit val equalStandardStep: Eq[StandardStep] = Eq.by(x =>
    (x.id, x.config, x.status, x.breakpoint, x.skip, x.fileId, x.configStatus, x.observeStatus)
  )
}

// Other kinds of Steps to be defined.
@Lenses
final case class NodAndShuffleStep(
  override val id:         StepId,
  override val config:     StepConfig,
  override val status:     StepState,
  override val breakpoint: Boolean,
  override val skip:       Boolean,
  override val fileId:     Option[ImageFileId],
  configStatus:            List[(Resource, ActionStatus)],
  nsStatus:                NodAndShuffleStatus,
  pendingObserveCmd:       Option[NodAndShuffleStep.PendingObserveCmd]
) extends Step

object NodAndShuffleStep {
  implicit val equalNodAndShuffleStop: Eq[NodAndShuffleStep] = Eq.by(x =>
    (x.id, x.config, x.status, x.breakpoint, x.skip, x.fileId, x.configStatus, x.nsStatus)
  )

  sealed trait PendingObserveCmd extends Product with Serializable
  case object PauseGracefully    extends PendingObserveCmd
  case object StopGracefully     extends PendingObserveCmd

  implicit val enumPendingObserveCmd: Enumerated[PendingObserveCmd] =
    Enumerated.of(PauseGracefully, StopGracefully)

}
