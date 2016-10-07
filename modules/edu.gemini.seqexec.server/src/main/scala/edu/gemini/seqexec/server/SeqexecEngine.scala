package edu.gemini.seqexec.server

import edu.gemini.pot.sp.SPObservationID

import scalaz.\/
import scalaz.concurrent.Task
import scalaz.stream.Process

/**
  * Created by jluhrs on 10/7/16.
  */
object SeqexecEngine {

  def start(seqId: SPObservationID): SeqexecFailure \/ Unit = ???
  def requestPause(seqId: SPObservationID): SeqexecFailure \/ Unit = ???
  def setBreakpoint(seqId: SPObservationID): SeqexecFailure \/ Unit = ???
  def setSkipMark(seqId: SPObservationID): SeqexecFailure \/ Unit = ???
  def requestRefresh(): Unit = ???
  def eventProcess(): Process[Task, SeqexecEvent] = ???


  sealed trait SeqexecEvent
  object SeqexecEvent {
    case class SequenceStart(view: SequenceView) extends SeqexecEvent
    case class StepExecuted(view: SequenceView) extends SeqexecEvent
    case class SequenceCompleted(view: SequenceView) extends SeqexecEvent
    case class StepBreakpointChanged(view: SequenceView) extends SeqexecEvent
    case class StepSkipMarkChanged(view: SequenceView) extends SeqexecEvent
    case class SequencePauseRequested(view: SequenceView) extends SeqexecEvent
    case class NewLogMessage(msg: LogMsg) extends SeqexecEvent
  }

  type SystemName = String
  type ParamName = String
  type ParamValue = String
  type Parameters = Map[ParamName, ParamValue]
  type StepConfig = Map[SystemName, Parameters]

  sealed trait StepState
  object StepState {
    object Pending extends StepState
    object Completed extends StepState
    object Skipped extends StepState
    case class Error(msg: String) extends StepState
    object Running extends StepState
  }

  trait ActionStatus
  object ActionStatus {
    object Pending
    object Completed
    case class Running(progress: Double)
  }

  abstract class Step {
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
  )  extends Step
  // Other kinds of Steps to be defined.

  sealed trait SequenceState
  object SequenceState {
    object Completed
    object Running
    object Idle
  }

  case class SequenceView (
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
