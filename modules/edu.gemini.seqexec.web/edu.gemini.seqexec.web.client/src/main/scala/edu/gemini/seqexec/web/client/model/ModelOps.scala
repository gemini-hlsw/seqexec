package edu.gemini.seqexec.web.client.model

import edu.gemini.seqexec.model.SharedModel.{SequenceState, SequenceView, StepState}

import scalaz.Show

/**
  * Contains useful operations for the seqexec model
  */
object ModelOps {
  implicit val sequenceStateShow = Show.shows[SequenceState] {
    case SequenceState.Completed => "Complete"
    case SequenceState.Running   => "Running"
    case SequenceState.Idle      => "Idle"
  }


  implicit class SequenceViewOps(val s: SequenceView) extends AnyVal {
    private def progress: (Int, Int) = (s.steps.count(_.status == StepState.Completed), s.steps.length)

    // Returns where on the sequence the execution is at
    def runningStep: Option[(Int, Int)] = s.status match {
      case SequenceState.Running    => Some(progress)
      //case SequenceState.Error      => Some(progress)
      case _                        => None
    }
  }
}
