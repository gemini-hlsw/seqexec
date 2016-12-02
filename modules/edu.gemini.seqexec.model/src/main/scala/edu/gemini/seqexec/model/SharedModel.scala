package edu.gemini.seqexec.model

object SharedModel {

  sealed trait SeqexecEvent
  object SeqexecEvent {
    case class ConnectionOpenEvent(u: Option[UserDetails]) extends SeqexecEvent

    case class SequenceStart(view: List[SequenceView]) extends SeqexecEvent

    case class StepExecuted(view: List[SequenceView]) extends SeqexecEvent

    case class SequenceCompleted(view: List[SequenceView]) extends SeqexecEvent

    case class SequenceLoaded(view: List[SequenceView]) extends SeqexecEvent

    case class StepBreakpointChanged(view: List[SequenceView]) extends SeqexecEvent

    case class StepSkipMarkChanged(view: List[SequenceView]) extends SeqexecEvent

    case class SequencePauseRequested(view: List[SequenceView]) extends SeqexecEvent

    // TODO: msg should be LogMsg bit it does IO when getting a timestamp, it
    // has to be embedded in a `Task`
    case class NewLogMessage(msg: String) extends SeqexecEvent

  }

  type SystemName = String
  type ParamName = String
  type ParamValue = String
  type Parameters = Map[ParamName, ParamValue]
  type StepConfig = Map[SystemName, Parameters]

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
    case object Completed extends SequenceState
    case object Running   extends SequenceState
    case object Idle      extends SequenceState
  }

  /**
    * Metadata about the sequence required on the exit point
    */
  // TODO Une a proper instrument class
  case class SequenceMetadata(instrument: String)

  case class SequenceView (
    metadata: SequenceMetadata,
    status: SequenceState,
    steps: List[Step],
    willStopIn: Option[Int]
  )

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
