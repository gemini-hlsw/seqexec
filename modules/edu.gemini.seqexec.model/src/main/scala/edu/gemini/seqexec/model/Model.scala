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

    //Generic update. It will probably become useless if we have a special Event for every case.
    case class SequenceUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

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
  type StepId = Int
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
    val id: StepId
    val config: StepConfig
    val status: StepState
    val breakpoint: Boolean
    val skip: Boolean
    val fileId: Option[dhs.ObsId]
  }

  case class StandardStep(
    override val id: StepId,
    override val config: StepConfig,
    override val status: StepState,
    override val breakpoint: Boolean,
    override val skip: Boolean,
    override val fileId: Option[dhs.ObsId],
    configStatus: Map[SystemName, ActionStatus],
    observeStatus: ActionStatus
  ) extends Step
  // Other kinds of Steps to be defined.

  sealed trait SequenceState
  object SequenceState {
    case object Completed         extends SequenceState
    case object Running           extends SequenceState
    case object Stopping          extends SequenceState
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
  )

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

  object SequenceOperations {
    case object PauseSequence extends SequenceOperations
    case object ContinueSequence extends SequenceOperations
    case object StopSequence extends SequenceOperations
    case object RunSequence extends SequenceOperations
  }

  // Operations possible at the observation level
  sealed trait ObservationOperations

  object ObservationOperations {
    case object PauseObservation extends ObservationOperations
    case object StopObservation extends ObservationOperations
    case object AbortObservation extends ObservationOperations
    case object ResumeObservation extends ObservationOperations
  }

  // Operations for Hamamatsu
  case object PauseImmediatelyObservation extends ObservationOperations
  case object StopImmediatelyObservation extends ObservationOperations
  case object PauseGracefullyObservation extends ObservationOperations
  case object StopGracefullyObservation extends ObservationOperations
}
