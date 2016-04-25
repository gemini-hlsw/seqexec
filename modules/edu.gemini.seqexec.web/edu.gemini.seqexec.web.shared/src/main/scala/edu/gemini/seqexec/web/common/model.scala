package edu.gemini.seqexec.web.common

import scalaz._
import Scalaz._

/**
  * Minimal models to be exchanged between client and server
  */
case class StepConfig(key: String, value: String)

sealed trait StepState

object StepState {
  case object NotDone extends StepState
  case object Running extends StepState
  case object Done    extends StepState
  case object Error   extends StepState
  case object Abort   extends StepState
  case object Paused  extends StepState

  implicit val show = Show.shows[StepState] {
    case Running    => "Running"
    case NotDone    => "Pending"
    case Done       => "Done"
    case Error      => "Error"
    case Abort      => "Aborted"
    case Paused     => "Pause"
  }

}

case class Step(id: Int, state: StepState, config: List[StepConfig], file: Option[String])
case class SequenceSteps(steps: List[Step]) {
  def progress: (Int, Int) = (steps.count(_.state == StepState.Done), steps.length)
  def stopAtNext: SequenceSteps = {
    val done = steps.count(_.state == StepState.Done) + 1
    copy(steps.collect {
      case s @ Step(i, _, _, _) if i === done => s.copy(state = StepState.Abort)
      case s                                  => s
    })
  }
}

sealed trait SequenceState

object SequenceState {
  case object NotRunning extends SequenceState
  case object Running    extends SequenceState
  case object Abort      extends SequenceState
  case object Error      extends SequenceState
  case object Completed  extends SequenceState

  implicit val show = Show.shows[SequenceState] {
    case Running    => "Running"
    case NotRunning => "Inactive"
    case Error      => "Error"
    case Abort      => "Aborted"
    case Completed  => "Done"
  }
}

case class Sequence(id: String, state: SequenceState, instrument: Instrument.Instrument, steps: SequenceSteps, error: Option[String]) {

  def stopAtCurrentStep: Sequence = state match {
    case SequenceState.Running => this.copy(state = SequenceState.Abort, error = "Sequence aborted".some, steps = steps.stopAtNext)
    case _                     => this
  }

  // Returns where on the sequence the execution is at
  def runningStep: Option[(Int, Int)] = state match {
    case SequenceState.Running    => Some(steps.progress)
    case SequenceState.Error      => Some(steps.progress)
    case _                        => None
  }
}

object Sequence {
  val stepsLens: Sequence @> SequenceSteps  = Lens.lensu((a, b) => a.copy(steps = b), _.steps)
  val stepsListLens: Sequence @> List[Step] = stepsLens >=> Lens.lensu((a, b) => a.copy(steps = b), _.steps)

  def step(i: Int): Sequence @?> Step       = stepsListLens.partial >=> PLens.listNthPLens[Step](i)
}

case class SeqexecQueue(queue: List[Sequence]) {
  // Modify step i's state using a lens
  private def markStep(s: Sequence,i: Int, state: StepState):Sequence = Sequence.step(i).mod(_.copy(state = state), s)

  def markStepRunning(s: Sequence, i: Int) = markStep(s, i, StepState.Running)
  // Modify step i marking it as running using a lens
  def markStepDone(s: Sequence, i: Int, fileId: String) = Sequence.step(i).mod(_.copy(file = fileId.some), markStep(s, i, StepState.Done))

  def stopSequence(id: String): SeqexecQueue = copy(queue.collect {
      case s @ Sequence(i, _, _, _, _) if i === id => s.stopAtCurrentStep
      case s                                       => s
    })

  // Update the sequence if found
  def markAsRunning(id: String): SeqexecQueue = copy(queue.collect {
      case s @ Sequence(i, _, _, _, _) if i === id => markStepRunning(s, 0).copy(state = SequenceState.Running)
      case s                                       => s
    })

  // Update the sequence as done if found
  def markAsCompleted(id: String): SeqexecQueue = copy(queue.collect {
      case s @ Sequence(i, _, _, _, _) if i === id => s.copy(state = SequenceState.Completed)
      case s                                       => s
    })

  // Update a step of a sequence
  def markStepAsCompleted(id: String, step: Int, fileId: String): SeqexecQueue = copy(queue.collect {
      case s @ Sequence(i, _, _, _, _) if i === id => Sequence.step(step).get(s) match {
        case Some(Step(_, StepState.Abort, _ ,_)) => markStepDone(s, step - 1, fileId)
        case _                                    => markStepRunning(markStepDone(s, step - 1, fileId), step)
      }
      case s                                       => s
    })
}

object Instrument {
  // Placeholder for the instrument type
  type Instrument = String

  // TODO Replace these for  a real list of instruments
  // TODO This list should be site-specific
  val instruments = NonEmptyList[Instrument]("F2", List[Instrument]("GMOS-S", "GPI", "GSAOI"): _*)
}