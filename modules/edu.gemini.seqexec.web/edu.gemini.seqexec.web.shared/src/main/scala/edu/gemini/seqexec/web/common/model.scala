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
  case object Paused  extends StepState
}

case class Step(id: Int, state: StepState, config: List[StepConfig], file: Option[String])
case class SequenceSteps(steps: List[Step]) {
  def completeStep(i: Int, file: String): SequenceSteps = copy(steps.collect {
    case s @ Step(sid, _, c, _) if sid === i => s.copy(state = StepState.Done, file = Some(file))
    case s                                   => s
  })
}

sealed trait SequenceState

object SequenceState {
  case object NotRunning extends SequenceState
  case object Running    extends SequenceState
  case object Error      extends SequenceState
  case object Completed  extends SequenceState
}

case class Sequence(id: String, state: SequenceState, instrument: Instrument.Instrument, steps: SequenceSteps, error: Option[Int])

case class SeqexecQueue(queue: List[Sequence]) {
  // Update the sequence if found
  def markAsRunning(id: String): SeqexecQueue = copy(queue.collect {
      case s @ Sequence(i, _, _, _, _) if i === id => s.copy(state = SequenceState.Running)
      case s                                       => s
    })

  // Update the sequence as done if found
  def markAsCompleted(id: String): SeqexecQueue = copy(queue.collect {
      case s @ Sequence(i, _, _, _, _) if i === id => s.copy(state = SequenceState.Completed)
      case s                                       => s
    })

  // Update a step of a sequence
  def markStepAsCompleted(id: String, step: Int, fileId: String): SeqexecQueue = copy(queue.collect {
      case s @ Sequence(i, _, _, _, _) if i === id => s.copy(steps = s.steps.completeStep(step, fileId))
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