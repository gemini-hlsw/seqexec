package edu.gemini.seqexec.web.common

import scalaz._
import Scalaz._

/**
  * Minimal models to be exchanged between client and server
  */
case class StepConfig(key: String, value: String)

sealed trait StepState

/**
  * Possible states for given step
  */
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

object Step {
  // Lenses
  val fileLens: Step @> Option[String] = Lens.lensu((s, f) => s.copy(file = f), s => s.file)
  val stateLens: Step @> StepState     = Lens.lensu((s, f) => s.copy(state = f), s => s.state)
}

case class SequenceSteps(steps: List[Step]) {
  /**
    * Find the currently running step
    */
  def progress: (Int, Int) = (steps.count(_.state == StepState.Done), steps.length)

  /**
    * Mark the next step as aborted
    */
  def stopAtNext: SequenceSteps = {
    // TODO This should be done at the engine level
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

  // Update the running state and mark the next step
  def abortAtNextStep: Sequence = state match {
    case SequenceState.Running => this.copy(state = SequenceState.Abort, error = "Sequence aborting".some, steps = steps.stopAtNext)
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
  // Lenses
  val stepsLens: Sequence @> SequenceSteps          = Lens.lensu((a, b) => a.copy(steps = b), _.steps)
  val stateLens: Sequence @> SequenceState          = Lens.lensu((a, b) => a.copy(state = b), _.state)
  val errorLens: Sequence @> Option[String]         = Lens.lensu((a, b) => a.copy(error = b), _.error)
  val stepsListLens: Sequence @> List[Step]         = stepsLens >=> Lens.lensu((a, b) => a.copy(steps = b), _.steps)

  def step(i: Int): Sequence @?> Step               = stepsListLens.partial >=> PLens.listNthPLens[Step](i)
  def stepFile(i: Int): Sequence @?> Option[String] = step(i) andThen Step.fileLens.partial
  def stepState(i: Int): Sequence @?> StepState     = step(i) andThen Step.stateLens.partial
}

case class SeqexecQueue(queue: List[Sequence]) {

  // Make a virtual abort
  // TODO implement abort at the engine level
  def abortSequence(id: String): SeqexecQueue = copy(queue.collect {
      case s @ Sequence(i, _, _, _, _) if i === id => s.abortAtNextStep
      case s                                       => s
    })

  // Set the state of the given step
  private def stepRunning(s: Sequence, i: Int):PState[Sequence, StepState] =
    Sequence.stepState(i) := StepState.Running

  // Update the sequence if found
  def sequenceRunning(id: String): SeqexecQueue = copy(queue.collect {
    case s @ Sequence(i, _, _, _, _) if i === id =>
      (for {
        _ <- stepRunning(s, 0)
        t <- Sequence.stateLens := SequenceState.Running
      } yield t).exec(s)
    case s                                       => s
  })

  // Update the sequence as done if found
  def sequenceCompleted(id: String): SeqexecQueue = copy(queue.collect {
      case s @ Sequence(i, _, _, _, _) if i === id => (Sequence.stateLens := SequenceState.Completed).exec(s)
      case s                                       => s
    })

  // Modify step i marking it as done and set the file id
  private def stepDone(s: Sequence, i: Int, fileId: String) =
    for {
      _ <- Sequence.stepState(i) := StepState.Done
      u <- Sequence.stepFile(i) := fileId.some
    } yield u

  private def stepAborted(s: Sequence, step: Int, fileId: String) =
    for {
      _ <- Sequence.errorLens := "Sequence aborted".some
      u <- stepDone(s, step - 1, fileId)
    } yield u

  private def advanceRunningStep(s: Sequence, step: Int, fileId: String) =
    for {
      _ <- stepRunning(s, step)
      u <- stepDone(s, step - 1, fileId)
    } yield u

  // Update a step of a sequence
  def markStepAsCompleted(id: String, step: Int, fileId: String): SeqexecQueue = copy(queue.collect {
      case s @ Sequence(i, _, _, _, _) if i === id => Sequence.step(step).flatMap {
        // The step maybe was aborted, in that case mark the sequence as aborted too
        case Some(Step(_, StepState.Abort, _, _)) =>
          stepAborted(s, step, fileId)
        case _ =>
          advanceRunningStep(s, step, fileId)
      }.exec(s)
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