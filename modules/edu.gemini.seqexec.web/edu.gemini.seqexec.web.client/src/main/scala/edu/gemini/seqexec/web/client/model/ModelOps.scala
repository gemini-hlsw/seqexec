// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client

import edu.gemini.seqexec.model.Model.{ActionStatus, Resource, Instrument, SequenceState, SequenceView, Step, StepState, StandardStep}

import scalaz.Show
import scalaz.syntax.equal._
import scalaz.syntax.show._
import scalaz.std.AllInstances._

/**
  * Contains useful operations for the seqexec model
  */
object ModelOps {
  implicit val sequenceStateShow: Show[SequenceState] = Show.shows[SequenceState] {
    case SequenceState.Completed     => "Complete"
    case SequenceState.Running(true, _) => "Stopping..."
    case SequenceState.Running(_, _) => "Running"
    case SequenceState.Idle          => "Idle"
    case SequenceState.Stopped       => "Stopped"
    case SequenceState.Failed(_)     => s"Error at step "
  }

  implicit val stepStateShow: Show[StepState] = Show.shows[StepState] {
    case StepState.Pending    => "Pending"
    case StepState.Completed  => "Done"
    case StepState.Skipped    => "Skipped"
    case StepState.Failed(msg) => s"Error $msg"
    case StepState.Running    => "Running"
    case StepState.Paused     => "Paused"
  }

  implicit val stepShow: Show[Step] = Show.shows[Step] { s =>
    s.status match {
      case StepState.Pending                    => "Pending"
      case StepState.Completed                  => "Done"
      case StepState.Skipped                    => "Skipped"
      case StepState.Failed(msg)                => s"Error $msg"
      case StepState.Running if s.isObserving   => "Observing..."
      case StepState.Running if s.isConfiguring => "Configuring..."
      case StepState.Running                    => "Running..."
      case StepState.Paused                     => "Paused"
    }
  }

  implicit val resourceShow: Show[Resource] = Show.shows[Resource] {
    case Resource.TCS    => "TCS"
    case Resource.Gcal   => "GCAL"
    case Resource.Gems   => "GeMS"
    case Resource.Altair => "Altair"
    case Resource.P1     => "P1"
    case Resource.OI     => "OI"
    case i: Instrument   => i.shows
  }

  implicit class SequenceStateOps(val s: SequenceState) extends AnyVal {
    def isError: Boolean = SequenceState.isError(s)

    def isInProcess: Boolean = s match {
      case SequenceState.Idle => false
      case _                  => true
    }

    def isRunning: Boolean = SequenceState.isRunning(s)
  }

  implicit class SequenceViewOps(val s: SequenceView) extends AnyVal {
    private def progress: (Int, Int) = (s.steps.count(_.status == StepState.Completed), s.steps.length)

    // Returns where on the sequence the execution is at
    def runningStep: Option[(Int, Int)] = s.status match {
      case SequenceState.Running(_, _) => Some(progress)
      case SequenceState.Failed(_)     => Some(progress)
      case _                           => None
    }

    def allStepsDone: Boolean = s.steps.forall(_.status === StepState.Completed)

    def flipStep(step: Step): SequenceView = s.copy(steps = s.steps.collect {
      case st: StandardStep if st == step => st.copy(skip = !st.skip)
      case st               => st
    })

    def flipBreakpointAtStep(step: Step): SequenceView = s.copy(steps = s.steps.collect {
      case st: StandardStep if st == step => st.copy(breakpoint = !st.breakpoint)
      case st               => st
    })

    def hasError: Boolean = SequenceState.isError(s.status)

    def nextStepToRun: Option[Int] =
      s.steps match {
        case x if x.forall(_.status === StepState.Pending)   => Some(0) // No steps have been executed, start at 0
        case x if x.forall(_.status === StepState.Completed) => None // All steps have been executed
        case x if x.exists(_.hasError)                       => Option(x.indexWhere((s: Step) => s.hasError)).filter(_ =/= -1)
        case x if x.exists(_.status === StepState.Paused)    => Option(x.indexWhere((s: Step) => s.status =/= StepState.Completed)).filter(_ =/= -1)
        case x                                               => Option(x.indexWhere((s: Step) => s.status =/= StepState.Completed)).filter(_ =/= -1)
      }

    def isPartiallyExecuted: Boolean = s.steps.exists(_.status === StepState.Completed)
  }

  implicit class StepOps(val s: Step) extends AnyVal {
    def flipBreakpoint: Step = s match {
      case st: StandardStep => st.copy(breakpoint = !st.breakpoint)
      case st               => st
    }

    def file: Option[String] = None

    def canSetBreakpoint: Boolean = s.status match {
      case StepState.Pending | StepState.Skipped | StepState.Paused => s.id > 0
      case _                                                        => false
    }

    def canSetSkipmark: Boolean = s.status match {
      case StepState.Pending | StepState.Skipped | StepState.Paused => true
      case _ if hasError                                            => true
      case _                                                        => false
    }

    def hasError: Boolean =
      s.status match {
        case StepState.Failed(_) => true
        case _                  => false
      }

    def isObserving: Boolean = s match {
      case StandardStep(_, _, _, _, _, _, _, o) => o === ActionStatus.Running
      case _                                    => false
    }

    def isConfiguring: Boolean = s match {
      case StandardStep(_, _, _, _, _, _, c, _) => c.map(_._2).contains(ActionStatus.Running)
      case _                                    => false
    }

  }
}
