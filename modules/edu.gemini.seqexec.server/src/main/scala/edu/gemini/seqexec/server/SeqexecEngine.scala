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

  // TODO: Add seqId: SPObservationID as parameter
  def start(q: engine.EventQueue, id: SPObservationID): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.start(id.stringValue())).map(_.right)

  // TODO: Add seqId: SPObservationID as parameter
  def requestPause(q: engine.EventQueue, id: SPObservationID): Task[SeqexecFailure \/ Unit ]=
    q.enqueueOne(Event.pause(id.stringValue())).map(_.right)

  // TODO: Add seqId: SPObservationID as parameter
  def setBreakpoint(q: engine.EventQueue, id: SPObservationID): Task[SeqexecFailure \/ Unit]= ???

  // TODO: Add seqId: SPObservationID as parameter
  def setSkipMark(q: engine.EventQueue, id: SPObservationID): Task[SeqexecFailure \/ Unit] = ???

  def requestRefresh(q: engine.EventQueue): Task[Unit] = q.enqueueOne(Event.poll)

  def eventProcess(q: engine.EventQueue): Process[Task, SeqexecEvent] =
    engine.process(q)(engine.initState).map {
      case (ev, qState) =>
        toSeqexecEvent(ev)(qState.values.map(s => viewSequence(s.toSequence)).toList)
    }

  def load(q: engine.EventQueue, seqId: SPObservationID): Task[SeqexecFailure \/ Unit] = {
    val t = EitherT( for {
        odbSeq <- Task(odbProxy.read(seqId))
      } yield odbSeq.flatMap(s => SeqTranslate.sequence(systems)(seqId.stringValue(), s))
    )
    val u = t.flatMapF(x => q.enqueueOne(Event.load(seqId.stringValue(), x)).map(_.right))
    u.run
  }

  private def toSeqexecEvent(ev: engine.Event)(svs: List[SequenceView]): SeqexecEvent = ev match {
    case engine.EventUser(ue) => ue match {
      case engine.Start(_)    => SequenceStart(svs)
      case engine.Pause(_)    => SequencePauseRequested(svs)
      case engine.Load(_, _)  => SequenceLoaded(svs)
      case engine.Poll        => NewLogMessage("Immediate State requested")
      case engine.Exit        => NewLogMessage("Exit requested by user")
    }
    case engine.EventSystem(se) => se match {
      // TODO: Sequence completed event not emited by engine.
      case engine.Completed(_, _, _) => NewLogMessage("Action completed")
      case engine.Failed(_, _, _)    => NewLogMessage("Action failed")
      case engine.Executed(_)        => StepExecuted(svs)
      case engine.Finished(_)        => NewLogMessage("Execution finished")
    }
  }

    // TODO: Better name and move it to `engine`
    type QueueAR = engine.Queue[engine.Action \/ engine.Result]
    type SequenceAR = engine.Sequence[engine.Action \/ engine.Result]
    type StepAR = engine.Step[engine.Action \/ engine.Result]


    def viewSequence(seq: SequenceAR): SequenceView =
      // TODO: Implement willStopIn
      SequenceView(seq.id, seq.metadata, statusSequence(seq), engineSteps(seq), None)

    private def statusSequence(seq: SequenceAR): SequenceState = engine.Sequence.status(seq)

    private def engineSteps(seq: SequenceAR): List[Step] = {

      def statusStep(step: StepAR): StepState = engine.Step.status(step)

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
