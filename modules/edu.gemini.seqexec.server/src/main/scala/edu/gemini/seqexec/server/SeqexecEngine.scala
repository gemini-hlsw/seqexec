package edu.gemini.seqexec.server

import java.time.LocalDate

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.engine
import edu.gemini.seqexec.engine.{Action, Event, Sequence}

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process
import knobs._
import edu.gemini.seqexec.model.SharedModel._
import edu.gemini.seqexec.model.SharedModel.SeqexecEvent._
import edu.gemini.spModel.core.Peer

/**
  * Created by jluhrs on 10/7/16.
  */
class SeqexecEngine(settings: SeqexecEngine.Settings) {

  val odbProxy = new ODBProxy(new Peer(settings.odbHost, 8443, null))

  val systems = SeqTranslate.Systems(
    if(settings.dhsSim) DhsClientSim(settings.date) else DhsClientHttp(settings.dhsURI),
    if(settings.tcsSim) TcsControllerSim else TcsControllerEpics,
    if(settings.instSim) Flamingos2ControllerSim else Flamingos2ControllerEpics
  )

  val qs: engine.Queue.State = engine.Queue.State.init(
    engine.Queue(
      List[Sequence[Action]](
      )
    )
  )

  // TODO: Add seqId: SPObservationID as parameter
  def start(q: engine.EventQueue): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.start).map(_.right)

  // TODO: Add seqId: SPObservationID as parameter
  def requestPause(q: engine.EventQueue): Task[SeqexecFailure \/ Unit ]=
    q.enqueueOne(Event.pause).map(_.right)

  // TODO: Add seqId: SPObservationID as parameter
  def setBreakpoint(q: engine.EventQueue): Task[SeqexecFailure \/ Unit]= ???

  // TODO: Add seqId: SPObservationID as parameter
  def setSkipMark(q: engine.EventQueue): Task[SeqexecFailure \/ Unit] = ???

  def requestRefresh(q: engine.EventQueue): Task[Unit] = q.enqueueOne(Event.poll)

  def eventProcess(q: engine.EventQueue): Process[Task, SeqexecEvent] =
    engine.process(q)(qs).map {
      case (ev, qState) =>
        toSeqexecEvent(ev)(qState.toQueue.sequences.map(viewSequence))
    }

  def load(q: engine.EventQueue, seqId: SPObservationID): Task[SeqexecFailure \/ Unit] = {
    val t = EitherT( for {
        odbSeq <- Task(odbProxy.read(seqId))
      } yield odbSeq.flatMap(s => SeqTranslate.sequence(systems)(seqId.stringValue(), s))
    )
    val u = t.flatMapF(x => q.enqueueOne(Event.load(x)).map(_.right))
    u.run
  }

  private def toSeqexecEvent(ev: engine.Event)(svs: List[SequenceView]): SeqexecEvent = ev match {
    case engine.EventUser(ue) => ue match {
      case engine.Start   => SequenceStart(svs)
      case engine.Pause   => SequencePauseRequested(svs)
      case engine.Load(_) => SequenceLoaded(svs)
      case engine.Poll    => NewLogMessage("Immediate State requested")
      case engine.Exit    => NewLogMessage("Exit requested by user")
    }
    case engine.EventSystem(se) => se match {
      // TODO: Sequence completed event not emited by engine.
      case engine.Completed(_, _) => NewLogMessage("Action completed")
      case engine.Failed(_, _)    => NewLogMessage("Action failed")
      case engine.Executed        => StepExecuted(svs)
      case engine.Finished        => NewLogMessage("Execution finished")
    }
  }

    // TODO: Better name and move it to `engine`
    type QueueAR = engine.Queue[engine.Action \/ engine.Result]
    type SequenceAR = engine.Sequence[engine.Action \/ engine.Result]
    type StepAR = engine.Step[engine.Action \/ engine.Result]


    def viewSequence(seq: SequenceAR): SequenceView =
      // TODO: Implement willStopIn
      SequenceView(seq.metadata, statusSequence(seq), engineSteps(seq), None)

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
          step.config,
          statusStep(step),
          // TODO: Implement breakpoints at Engine level
          breakpoint = false,
          // TODO: Implement skipping at Engine level
          skip = false,
          Map.empty,
          // TODO: Implement standard step at Engine level
          ActionStatus.Pending
        )

      seq.steps.map(viewStep)
    }

    // Configuration stuff


}

object SeqexecEngine {

  case class Settings(odbHost: String,
                      date: LocalDate,
                      dhsURI: String,
                      dhsSim: Boolean,
                      tcsSim: Boolean,
                      instSim: Boolean,
                      gcalSim: Boolean)

  def apply(settings: Settings) = new SeqexecEngine(settings)


  def seqexecConfiguration: Kleisli[Task, Config, Settings] = Kleisli { cfg: Config => {
      val odbHost = cfg.require[String]("seqexec-engine.odb")
      val dhsServer = cfg.require[String]("seqexec-engine.dhsServer")
      val dhsSim = cfg.require[Boolean]("seqexec-engine.dhsSim")
      val tcsSim = cfg.require[Boolean]("seqexec-engine.tcsSim")
      val instSim = cfg.require[Boolean]("seqexec-engine.instSim")
      val gcalSim = cfg.require[Boolean]("seqexec-engine.gcalSim")

      for {
        now <- Task(LocalDate.now)
      } yield Settings(odbHost, now, dhsServer, dhsSim, tcsSim, instSim, gcalSim)

    }
  }

}
