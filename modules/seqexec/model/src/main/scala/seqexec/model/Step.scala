// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import cats.implicits._
import seqexec.model.enum._
import seqexec.model.Model.{ StepConfig, StepId }

sealed trait Step {
  val id: StepId
  val config: StepConfig
  val status: StepState
  val breakpoint: Boolean
  val skip: Boolean
  val fileId: Option[dhs.ImageFileId]
}
object Step {
  val Zero: Step = StandardStep(id = -1, config = Map.empty, status = StepState.Pending, breakpoint = false, skip = false, fileId = None, configStatus = Nil, observeStatus = ActionStatus.Pending)

  implicit val equal: Eq[Step] =
    Eq.by { x =>
      (x.id, x.config, x.status, x.breakpoint, x.skip, x.fileId)
    }

  implicit class StepOps(val s: Step) extends AnyVal {
    def flipBreakpoint: Step = s match {
      case st: StandardStep => st.copy(breakpoint = !st.breakpoint)
      case st               => st
    }

    def flipSkip: Step = s match {
      case st: StandardStep => st.copy(skip = !st.skip)
      case st               => st
    }

    def file: Option[String] = None

    def canSetBreakpoint(i: Int, firstRunnable: Int): Boolean = s.status match {
      case StepState.Pending | StepState.Skipped | StepState.Paused => i > firstRunnable
      case _                                                        => false
    }

    def canSetSkipmark: Boolean = s.status match {
      case StepState.Pending | StepState.Paused => true
      case _ if hasError                        => true
      case _                                    => false
    }

    def hasError: Boolean = s.status match {
      case StepState.Failed(_) => true
      case _                  => false
    }

    def isObserving: Boolean = s match {
      case StandardStep(_, _, _, _, _, _, _, o) => o === ActionStatus.Running
      case _                                    => false
    }

    def isObservePaused: Boolean = s match {
      case StandardStep(_, _, _, _, _, _, _, o) => o === ActionStatus.Paused
      case _                                    => false
    }

    def isConfiguring: Boolean = s match {
      case StandardStep(_, _, _, _, _, _, c, _) => c.map(_._2).contains(ActionStatus.Running)
      case _                                    => false
    }

    def isFinished: Boolean = s.status === StepState.Completed || s.status === StepState.Skipped

    def wasSkipped: Boolean = s.status === StepState.Skipped

  }
}

final case class StandardStep(
  override val id: StepId,
  override val config: StepConfig,
  override val status: StepState,
  override val breakpoint: Boolean,
  override val skip: Boolean,
  override val fileId: Option[dhs.ImageFileId],
  configStatus: List[(Resource, ActionStatus)],
  observeStatus: ActionStatus
) extends Step
object StandardStep {
  implicit val equal: Eq[StandardStep] =
    Eq.by { x =>
      (x.id, x.config, x.status, x.breakpoint, x.skip, x.fileId, x.configStatus, x.observeStatus)
    }
}
// Other kinds of Steps to be defined.