package edu.gemini.seqexec.web.client.model

import edu.gemini.seqexec.model.Model.{SequenceState, SequenceView, Step, StepState, StandardStep}

import scalaz.Show

/**
  * Contains useful operations for the seqexec model
  */
object ModelOps {
  implicit val sequenceStateShow: Show[SequenceState] = Show.shows[SequenceState] {
    case SequenceState.Completed => "Complete"
    case SequenceState.Running   => "Running"
    case SequenceState.Idle      => "Idle"
    case SequenceState.Paused    => "Paused"
    case SequenceState.Error(e)  => s"Error $e"
  }

  implicit val steStateShow: Show[StepState] = Show.shows[StepState] {
    case StepState.Pending    => "Pending"
    case StepState.Completed  => "Done"
    case StepState.Skipped    => "Skipped"
    case StepState.Error(msg) => s"Error $msg"
    case StepState.Running    => "Running"
    case StepState.Paused     => "Paused"
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

    def flipBreakpaintAtStep(step: Step): SequenceView = s.copy(steps = s.steps.collect {
      case st: StandardStep if st == step => st.copy(breakpoint = !st.breakpoint)
      case st               => st
    })

    def hasError: Boolean =
      s.status match {
        case SequenceState.Error(_) => true
        case _                      => false
      }
  }

  implicit class StepOps(val s: Step) extends AnyVal {
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
