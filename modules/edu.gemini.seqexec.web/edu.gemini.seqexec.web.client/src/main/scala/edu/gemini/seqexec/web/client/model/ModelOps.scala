package edu.gemini.seqexec.web.client.model

import edu.gemini.seqexec.model.Model.{SequenceState, SequenceView, Step, StepState, StandardStep}
import edu.gemini.seqexec.model.Model.ObservationOperations
import edu.gemini.seqexec.model.Model.SequenceOperations

import scalaz.Show

/**
  * Contains useful operations for the seqexec model
  */
object ModelOps {
  implicit val sequenceStateShow: Show[SequenceState] = Show.shows[SequenceState] {
    case SequenceState.Completed => "Complete"
    case SequenceState.Running   => "Running"
    case SequenceState.Stopping  => "Stopping"
    case SequenceState.Idle      => "Idle"
    case SequenceState.Paused    => "Paused"
    case SequenceState.Error(_)  => s"Error at step "
  }

  implicit val stepStateShow: Show[StepState] = Show.shows[StepState] {
    case StepState.Pending    => "Pending"
    case StepState.Completed  => "Done"
    case StepState.Skipped    => "Skipped"
    case StepState.Error(msg) => s"Error $msg"
    case StepState.Running    => "Running"
    case StepState.Paused     => "Paused"
  }

  implicit class SequenceStateOps(val s: SequenceState) extends AnyVal {
    def hasError: Boolean = s match {
      case SequenceState.Error(_) => true
      case _                      => false
    }

    def isInProcess: Boolean = s match {
      case SequenceState.Idle => false
      case _                  => true
    }
  }

  implicit class SequenceViewOps(val s: SequenceView) extends AnyVal {
    private def progress: (Int, Int) = (s.steps.count(_.status == StepState.Completed), s.steps.length)

    // Returns where on the sequence the execution is at
    def runningStep: Option[(Int, Int)] = s.status match {
      case SequenceState.Running    => Some(progress)
      case SequenceState.Error(_)   => Some(progress)
      case _                        => None
    }

    def allStepsDone: Boolean = s.steps.forall(_.status == StepState.Completed)

    def flipStep(step: Step): SequenceView = s.copy(steps = s.steps.collect {
      case st: StandardStep if st == step => st.copy(skip = !st.skip)
      case st               => st
    })

    /**
     * Returns the observation operations allowed
     * TODO Convert to an Instrument-level typeclass
     */
    def allowedObservationOperations(step: Step): List[ObservationOperations] =
      s.metadata.instrument match {
        case _                                                 => Nil
      }

    /**
     * Returns the observation operations allowed
     * TODO Convert to an Instrument-level typeclass
     */
    def allowedSequenceOperations: List[SequenceOperations] = Nil

    def flipBreakpointAtStep(step: Step): SequenceView = s.copy(steps = s.steps.collect {
      case st: StandardStep if st == step => st.copy(breakpoint = !st.breakpoint)
      case st               => st
    })

    def hasError: Boolean =
      s.status match {
        case SequenceState.Error(_) => true
        case _                      => false
      }

    def nextStepToRun: Option[Int] =
      s.steps match {
        case x if x.forall(_.status == StepState.Pending)   => Some(0) // No steps have been executed, start at 0
        case x if x.forall(_.status == StepState.Completed) => None // All steps have been executed
        case x if x.exists(_.hasError)                      => Option(x.indexWhere((s: Step) => s.hasError)).filter(_ != -1).map(_ + 1)
        case x if x.exists(_.status == StepState.Paused)    => Option(x.indexWhere((s: Step) => s.status != StepState.Completed)).filter(_ != -1)
        case x                                              => Option(x.indexWhere((s: Step) => s.status != StepState.Completed)).filter(_ != -1)
      }

    def isPartiallyExecuted: Boolean = s.steps.exists(_.status == StepState.Completed)
  }

  implicit class StepOps(val s: Step) extends AnyVal {
    def flipBreakpoint: Step = s match {
      case st: StandardStep => st.copy(breakpoint = !st.breakpoint)
      case st               => st
    }

    def file: Option[String] = None

    def canSetBreakpoint: Boolean = s.status match {
      case StepState.Pending | StepState.Skipped | StepState.Paused => true
      case _ if hasError                                            => true
      case _                                                        => false
    }

    def canSetSkipmark: Boolean = s.status match {
      case StepState.Pending | StepState.Skipped | StepState.Paused => true
      case _ if hasError                                            => true
      case _                                                        => false
    }

    def hasError: Boolean =
      s.status match {
        case StepState.Error(_) => true
        case _                  => false
      }
  }
}
