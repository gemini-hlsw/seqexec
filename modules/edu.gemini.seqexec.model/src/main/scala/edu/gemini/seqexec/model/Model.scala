package edu.gemini.seqexec.model

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

  sealed trait StepState
  object StepState {
    case object Pending extends StepState
    case object Completed extends StepState
    case object Skipped extends StepState
    case class Error(msg: String) extends StepState
    case object Running extends StepState
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
  }

  case class StandardStep(
    override val config: StepConfig,
    override val status: StepState,
    override val breakpoint: Boolean,
    override val skip: Boolean,
    configStatus: Map[SystemName, ActionStatus],
    observeStatus: ActionStatus
  ) extends Step
  // Other kinds of Steps to be defined.

  sealed trait SequenceState
  object SequenceState {
    case object Completed         extends SequenceState
    case object Running           extends SequenceState
    case object Idle              extends SequenceState
    case class Error(msg: String) extends SequenceState
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
}
