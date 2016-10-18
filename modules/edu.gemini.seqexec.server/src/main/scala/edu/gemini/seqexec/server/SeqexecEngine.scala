package edu.gemini.seqexec.server

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.engine
import edu.gemini.seqexec.engine.Event

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

    case class SequenceStart(view: List[SequenceView]) extends SeqexecEvent
    case class StepExecuted(view: List[SequenceView]) extends SeqexecEvent
    case class SequenceCompleted(view: List[SequenceView]) extends SeqexecEvent
    case class StepBreakpointChanged(view: List[SequenceView]) extends SeqexecEvent
    case class StepSkipMarkChanged(view: List[SequenceView]) extends SeqexecEvent
    case class SequencePauseRequested(view: List[SequenceView]) extends SeqexecEvent
    // TODO: msg should be LogMsg bit it does IO when getting a timestamp, it
    // has to be embedded in a `Task`
    case class NewLogMessage(msg: String) extends SeqexecEvent

    def toSeqexecEvent(ev: Event.Event)(svs: List[SequenceView]): SeqexecEvent = ev match {
      case Event.EventUser(ue) => ue match {
        case Event.Start => SequenceStart(svs)
        case Event.Pause => SequencePauseRequested(svs)
        case Event.Poll  => NewLogMessage("Immediate State requested")
        case Event.Exit  => NewLogMessage("Exit requested by user")
      }
      case Event.EventSystem(se) => se match {
        // TODO: Sequence completed event not emited by engine.
        case Event.Completed(_, _) => NewLogMessage("Action completed")
        case Event.Failed(_, _)    => NewLogMessage("Action failed")
        case Event.Executed        => StepExecuted(svs)
        case Event.Finished        => NewLogMessage("Execution finished")
      }
    }

    def process(q: engine.EventQueue): Process[engine.Engine, SeqexecEvent] =
      engine.Handler.handler(q).map {
        case (ev, qs) => toSeqexecEvent(ev)(qs.toQueue.sequences.map(SequenceView.viewSequence))
      }
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

  sealed trait ActionStatus
  object ActionStatus {
    case object Pending extends ActionStatus
    case object Completed extends ActionStatus
    case class Running(progress: Double) extends ActionStatus
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
    case object Completed extends SequenceState
    case object Running   extends SequenceState
    case object Idle      extends SequenceState
  }

  case class SequenceView (
    status: SequenceState,
    steps: List[Step],
    willStopIn: Option[Int]
  )

  object SequenceView {

    // TODO: Better name and move it to `engine`
    type QueueAR = engine.Queue[engine.Action \/ engine.Result]
    type SequenceAR = engine.Sequence[engine.Action \/ engine.Result]
    type StepAR = engine.Step[engine.Action \/ engine.Result]

    def viewSequence(seq: SequenceAR): SequenceView =
      // TODO: Implement willStopIn
      SequenceView(statusSequence(seq), engineSteps(seq), None)

    private def statusSequence(seq: SequenceAR): SequenceState =
      engine.Sequence.status(seq) match {
          case engine.Status.Waiting   => SequenceState.Idle
          case engine.Status.Completed => SequenceState.Completed
          case engine.Status.Running   => SequenceState.Running
        }

    private def engineSteps(seq: SequenceAR): List[Step] = {

      def statusStep(step: StepAR): StepState =
        engine.Step.status(step) match {
          case engine.Status.Waiting   => StepState.Pending
          case engine.Status.Completed => StepState.Completed
          case engine.Status.Running   => StepState.Running
        }

      def viewStep(step: StepAR): Step =
        StandardStep(
          // TODO: Add configuration parameter to Engine Step
          Map.empty,
          statusStep(step),
          // TODO: Implement breakpoints at Engine level
          false,
          // TODO: Implement skipping at Engine level
          false,
          Map.empty,
          // TODO: Implement standard step at Engine level
          ActionStatus.Pending
        )

      seq.steps.map(viewStep)
    }
  }

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
