package edu.gemini.seqexec.model

import scalaz._
import Scalaz._

object Model {

  sealed trait SeqexecEvent
  sealed trait SeqexecModelUpdate extends SeqexecEvent {
    def view: SequencesQueue[SequenceView]
  }
  object SeqexecEvent {
    case class ConnectionOpenEvent(u: Option[UserDetails]) extends SeqexecEvent

    case class SequenceStart(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class StepExecuted(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class SequenceCompleted(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class SequenceLoaded(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class StepBreakpointChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class StepSkipMarkChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class SequencePauseRequested(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class SequenceRefreshed(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    // TODO: msg should be LogMsg bit it does IO when getting a timestamp, it
    // has to be embedded in a `Task`
    case class NewLogMessage(msg: String) extends SeqexecEvent

  }

  type SystemName = String
  type ParamName = String
  type ParamValue = String
  type Parameters = Map[ParamName, ParamValue]
  type StepConfig = Map[SystemName, Parameters]
  // TODO This should be a richer type
  type SequenceId = String
  type Instrument = String

  implicit val equal: Equal[SequenceId] = Equal.equalA[SequenceId]

  sealed trait StepState {
    def canRunFrom: Boolean = false
  }
  object StepState {
    case object Pending extends StepState {
      override val canRunFrom = true
    }
    case object Completed extends StepState
    case object Skipped extends StepState
    case class Error(msg: String) extends StepState {
      override val canRunFrom = true
    }
    case object Running extends StepState
    case object Paused extends StepState {
      override val canRunFrom = true
    }

    implicit val equal: Equal[StepState] = Equal.equalA[StepState]
  }

  sealed trait ActionStatus
  object ActionStatus {
    case object Pending extends ActionStatus
    case object Completed extends ActionStatus
    case class Running(progress: Double) extends ActionStatus
  }

  sealed trait Step {
    val config: StepConfig
    val status: StepState
    val breakpoint: Boolean
    val skip: Boolean
    val configStatus: Map[SystemName, ActionStatus]
    val observeStatus: ActionStatus
  }

  case class StandardStep(
    override val config: StepConfig,
    override val status: StepState,
    override val breakpoint: Boolean,
    override val skip: Boolean,
    override val configStatus: Map[SystemName, ActionStatus],
    override val observeStatus: ActionStatus
  ) extends Step
  // Other kinds of Steps to be defined.

  sealed trait SequenceState
  object SequenceState {
    case object Completed         extends SequenceState
    case object Running           extends SequenceState
    case object Idle              extends SequenceState
    case object Paused            extends SequenceState
    case class Error(msg: String) extends SequenceState

    implicit val equal: Equal[SequenceState] = Equal.equalA[SequenceState]
  }

  /**
    * Metadata about the sequence required on the exit point
    */
  // TODO Une a proper instrument class
  case class SequenceMetadata(instrument: Instrument)

  case class SequenceView (
    id: SequenceId,
    metadata: SequenceMetadata,
    status: SequenceState,
    steps: List[Step],
    willStopIn: Option[Int]
  ) {
    /**
     * Returns the observation operations allowed
     * TODO Convert to an Instrument-level typeclass
     */
    def allowedObservationOperations(stepStatus: StepState): List[ObservationOperations] =
      metadata.instrument match {
        // Note the F2 doesn't suppor these operations but we'll simulate them
        // for demonstration purposes
        //case "Flamingos2" if status == SequenceState.Running => List(PauseImmediatelyObservation, PauseGracefullyObservation, StopImmediatelyObservation, StopGracefullyObservation, AbortObservation)
        // Regular instrument that support pause/stop/abort
        case "Flamingos2" if status == SequenceState.Running => List(PauseObservation, StopObservation, AbortObservation)
        case "Flamingos2" if stepStatus == StepState.Paused  => List(ResumeObservation, StopObservation, AbortObservation)
        case _                                               => Nil
      }

    /**
     * Returns the observation operations allowed
     * TODO Convert to an Instrument-level typeclass
     */
    def allowedSequenceOperations: List[SequenceOperations] = Nil

    def nextStepToRun: Option[Int] =
      steps match {
        case x if x.forall(_.status == StepState.Pending)   => Some(0) // No steps have been executed, start at 0
        case x if x.forall(_.status == StepState.Completed) => None // All steps have been executed
        case x if x.exists(_.status == StepState.Paused)    => Option(x.indexWhere((s: Step) => s.status != StepState.Completed)).filter(_ != -1).map(_ + 1)
        case x                                              => Option(x.indexWhere((s: Step) => s.status != StepState.Completed)).filter(_ != -1)
      }

    def isPartiallyExecuted: Boolean = steps.exists(_.status == StepState.Completed)
  }

  /**
    * Represents a queue with different levels of details. E.g. it could be a list of Ids
    * Or a list of fully hydrated SequenceViews
    */
  case class SequencesQueue[T](queue: List[T])

  // Log message types
  type Time = java.time.Instant

  trait LogType
  object LogType {
    object Debug
    object Info
    object Warning
    object Error
  }

  case class LogMsg(t: LogType, timestamp: Time, msg: String)

  // Operations possible at the sequence level
  sealed trait SequenceOperations
  case object PauseSequence extends SequenceOperations
  case object ContinueSequence extends SequenceOperations
  case object StopSequence extends SequenceOperations
  case object RunSequence extends SequenceOperations

  // Operations possible at the observation level
  sealed trait ObservationOperations
  case object PauseObservation extends ObservationOperations
  case object StopObservation extends ObservationOperations
  case object AbortObservation extends ObservationOperations
  case object ResumeObservation extends ObservationOperations
  // Operations for Hamamatsu
  case object PauseImmediatelyObservation extends ObservationOperations
  case object StopImmediatelyObservation extends ObservationOperations
  case object PauseGracefullyObservation extends ObservationOperations
  case object StopGracefullyObservation extends ObservationOperations
}
