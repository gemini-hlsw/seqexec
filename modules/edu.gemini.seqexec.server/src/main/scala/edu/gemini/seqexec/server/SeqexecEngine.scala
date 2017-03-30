package edu.gemini.seqexec.server

import java.time.LocalDate

import edu.gemini.epics.acm.CaService
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.model.p1.immutable.Site
import edu.gemini.seqexec.{engine, server}
import edu.gemini.seqexec.engine.{Engine, Event, EventSystem, Executed, Failed, Result, Sequence}

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process
import knobs._
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.model.Model.SeqexecEvent._
import edu.gemini.spModel.core.Peer

/**
  * Created by jluhrs on 10/7/16.
  */
class SeqexecEngine(settings: SeqexecEngine.Settings) {

  val odbProxy = new ODBProxy(new Peer(settings.odbHost, 8443, null),
    if (settings.odbNotifications) ODBProxy.OdbCommandsImpl(new Peer(settings.odbHost, 8442, null))
    else ODBProxy.DummyOdbCommands)

  val systems = SeqTranslate.Systems(
    odbProxy,
    if (settings.dhsSim) DhsClientSim(settings.date) else DhsClientHttp(settings.dhsURI),
    if (settings.tcsSim) TcsControllerSim else TcsControllerEpics,
    if (settings.gcalSim) GcalControllerSim else GcalControllerEpics,
    if (settings.instSim) {
      if (settings.instForceError) Flamingos2ControllerSimBad
      else Flamingos2ControllerSim
    } else Flamingos2ControllerEpics
  )

  val translatorSettings = SeqTranslate.Settings(tcsKeywords = settings.tcsKeywords, f2Keywords = settings.f2Keywords, gwsKeywords = settings.gwsKeywords)

  val translator = SeqTranslate(settings.site, systems, translatorSettings)

  def start(q: engine.EventQueue, id: SPObservationID): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.start(id.stringValue())).map(_.right)

  def requestPause(q: engine.EventQueue, id: SPObservationID): Task[SeqexecFailure \/ Unit ]=
    q.enqueueOne(Event.pause(id.stringValue())).map(_.right)

  def setBreakpoint(q: engine.EventQueue,
                    seqId: SPObservationID,
                    stepId: edu.gemini.seqexec.engine.Step.Id,
                    v: Boolean): Task[SeqexecFailure \/ Unit]=
    q.enqueueOne(Event.breakpoint(seqId.stringValue(), stepId, v)).map(_.right)

  def setOperator(q: engine.EventQueue, name: String): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setOperator(name)).map(_.right)

  def setObserver(q: engine.EventQueue,
                  seqId: SPObservationID,
                  name: String): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setObserver(seqId.stringValue(), name)).map(_.right)

  def setConditions(q: engine.EventQueue, conditions: Conditions): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setConditions(conditions)).map(_.right)

  def setImageQuality(q: engine.EventQueue, iq: ImageQuality): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setImageQuality(iq)).map(_.right)

  def setWaterVapor(q: engine.EventQueue, wv: WaterVapor): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setWaterVapor(wv)).map(_.right)

  def setSkyBackground(q: engine.EventQueue, sb: SkyBackground): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setSkyBackground(sb)).map(_.right)

  def setCloudCover(q: engine.EventQueue, cc: CloudCover): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setCloudCover(cc)).map(_.right)

  // TODO: Add seqId: SPObservationID as parameter
  def setSkipMark(q: engine.EventQueue, id: SPObservationID, stepId: edu.gemini.seqexec.engine.Step.Id): Task[SeqexecFailure \/ Unit] = ???

  def requestRefresh(q: engine.EventQueue): Task[Unit] = q.enqueueOne(Event.poll)

  def eventProcess(q: engine.EventQueue): Process[Task, SeqexecEvent] =
    engine.process(q)(Engine.State.empty).flatMap(x => Process.eval(notifyODB(x))).map {
      case (ev, qState) =>
        toSeqexecEvent(ev)(
          SequencesQueue(
            qState.conditions,
            qState.sequences.values.map(
              s => viewSequence(s.toSequence, s.status)
            ).toList
          )
        )
    }

  private def notifyODB(i: (Event, Engine.State)): Task[(Event, Engine.State)] = {
    def safeGetObsId(ids: String): SeqAction[SPObservationID] = EitherT(Task.delay(new SPObservationID(ids)).attempt.map(_.leftMap(e => SeqexecFailure.SeqexecException(e))))

    (i match {
      case (EventSystem(Failed(id, _, e)), _) => for {
          obsId <- safeGetObsId(id)
          _     <- systems.odb.obsAbort(obsId, e.msg)
        } yield ()
       case (EventSystem(Executed(id)), st) if st.sequences.get(id).map(
         _.status === SequenceState.Idle
       ).getOrElse(false) => for {
         obsId <- safeGetObsId(id)
         _     <- systems.odb.obsPause(obsId, "Sequence paused by user")
       } yield ()
      case _ => SeqAction(())
    }).run.map(_ => i)
  }

  def load(q: engine.EventQueue, seqId: SPObservationID): Task[SeqexecFailure \/ Unit] = {
    val t = EitherT( for {
        odbSeq <- Task(odbProxy.read(seqId))
      } yield odbSeq.flatMap(s => translator.sequence(translatorSettings)(seqId, s))
    )
    val u = t.flatMapF(x => q.enqueueOne(Event.load(seqId.stringValue(), x)).map(_.right))
    u.run
  }

  private def toSeqexecEvent(ev: engine.Event)(svs: SequencesQueue[SequenceView]): SeqexecEvent = ev match {
    case engine.EventUser(ue) => ue match {
      case engine.Start(_)            => SequenceStart(svs)
      case engine.Pause(_)            => SequencePauseRequested(svs)
      case engine.Load(_, _)          => SequenceLoaded(svs)
      case engine.Breakpoint(_, _, _) => StepBreakpointChanged(svs)
      case engine.SetOperator(_)      => OperatorUpdated(svs)
      case engine.SetObserver(_, _)   => ObserverUpdated(svs)
      case engine.SetConditions(_)    => ConditionsUpdated(svs)
      case engine.SetImageQuality(_)  => ConditionsUpdated(svs)
      case engine.SetWaterVapor(_)    => ConditionsUpdated(svs)
      case engine.SetSkyBackground(_) => ConditionsUpdated(svs)
      case engine.SetCloudCover(_)    => ConditionsUpdated(svs)
      case engine.Poll                => SequenceRefreshed(svs)
      case engine.Exit                => NewLogMessage("Exit requested by user")
    }
    case engine.EventSystem(se) => se match {
      // TODO: Sequence completed event not emited by engine.
      case engine.Completed(_, _, _)     => NewLogMessage("Action completed")
      case engine.PartialResult(_, _, _) => SequenceUpdated(svs)
      case engine.Failed(_, _, _)        => NewLogMessage("Action failed")
      case engine.Executed(_)            => StepExecuted(svs)
      case engine.Executing(_)           => NewLogMessage("Executing")
      case engine.Finished(_)            => SequenceCompleted(svs)
    }
  }

    // TODO: Better name and move it to `engine`
    type SequenceAR = engine.Sequence[engine.Action \/ engine.Result]
    type StepAR = engine.Step[engine.Action \/ engine.Result]

    def viewSequence(seq: SequenceAR, st: SequenceState): SequenceView = {

      def engineSteps(seq: SequenceAR): List[Step] = {

        def viewStep(step: StepAR): StandardStep =
          StandardStep(
            step.id,
            step.config,
            engine.Step.status(step),
            // TODO: Implement breakpoints at Engine level
            breakpoint = step.breakpoint,
            // TODO: Implement skipping at Engine level
            skip = false,
            configStatus = Map.empty,
            // TODO: Implement standard step at Engine level
            observeStatus = ActionStatus.Pending,
            fileId = step.fileId
          )

        // Couldn't find this on Scalaz
        def splitWhere[A](l: List[A])(p: (A => Boolean)): (List[A], List[A]) =
          l.splitAt(l.indexWhere(p))

        // TODO: Calculate the whole status here and remove `Engine.Step.status`
        // This will be easier once the exact status labels in the UI are fixed.
        seq.steps.map(viewStep) match {
          // Find first Pending Step when no Step is Running and mark it as Running
          case steps if (st === SequenceState.Running || st === SequenceState.Stopping) && steps.all(_.status =/= StepState.Running) =>
            val (xs, (y :: ys)) = splitWhere(steps)(_.status === StepState.Pending)
            xs ++ (y.copy(status = StepState.Running) :: ys)
          case steps if st === SequenceState.Idle && steps.any(_.status === StepState.Running) =>
            val (xs, (y :: ys)) = splitWhere(steps)(_.status === StepState.Running)
            xs ++ (y.copy(status = StepState.Paused) :: ys)
          case x => x
        }
      }
      // TODO: Implement willStopIn
      SequenceView(seq.id, seq.metadata, st, engineSteps(seq), None)
    }
}

// Configuration stuff
object SeqexecEngine {

  case class Settings(site: Site,
                      odbHost: String,
                      date: LocalDate,
                      dhsURI: String,
                      dhsSim: Boolean,
                      tcsSim: Boolean,
                      instSim: Boolean,
                      gcalSim: Boolean,
                      odbNotifications: Boolean,
                      tcsKeywords: Boolean,
                      f2Keywords: Boolean,
                      gwsKeywords: Boolean,
                      instForceError: Boolean)
  val defaultSettings = Settings(Site.GS, "localhost", LocalDate.of(2017, 1,1), "http://localhost/", true,
    true, true, true, false, false, false, false, false)

  def apply(settings: Settings) = new SeqexecEngine(settings)

  def seqexecConfiguration: Kleisli[Task, Config, Settings] = Kleisli { cfg: Config => {
      val site = cfg.require[String]("seqexec-engine.site") match {
        case "GS" => Site.GS
        case "GN" => Site.GN
      }
      val odbHost = cfg.require[String]("seqexec-engine.odb")
      val dhsServer = cfg.require[String]("seqexec-engine.dhsServer")
      val dhsSim = cfg.require[Boolean]("seqexec-engine.dhsSim")
      val tcsSim = cfg.require[Boolean]("seqexec-engine.tcsSim")
      val instSim = cfg.require[Boolean]("seqexec-engine.instSim")
      val gcalSim = cfg.require[Boolean]("seqexec-engine.gcalSim")
      val odbNotifications = cfg.require[Boolean]("seqexec-engine.odbNotifications")
      val tcsKeywords = cfg.require[Boolean]("seqexec-engine.tcsKeywords")
      val f2Keywords = cfg.require[Boolean]("seqexec-engine.f2Keywords")
      val gwsKeywords = cfg.require[Boolean]("seqexec-engine.gwsKeywords")
      val instForceError = cfg.require[Boolean]("seqexec-engine.instForceError")

    // TODO: Review initialization of EPICS systems
    def initEpicsSystem(sys: EpicsSystem): Task[Unit] = Task(Option(CaService.getInstance()) match {
          case None => throw new Exception("Unable to start EPICS service.")
          case Some(s) => {
            sys.init(s).leftMap {
                case SeqexecFailure.SeqexecException(ex) => throw ex
                case c: SeqexecFailure => throw new Exception(SeqexecFailure.explain(c))
            }
          }
        }
      )

      val tcsInit = if(tcsKeywords || !tcsSim) initEpicsSystem(TcsEpics) else Task.now(())
      // More instruments to be added to the list here
      val instInit = if(f2Keywords || !instSim)
        Nondeterminism[Task].gatherUnordered(List(Flamingos2Epics).map(initEpicsSystem(_)))
      else Task.now(())
      val gwsInit = if(gwsKeywords) initEpicsSystem(GwsEpics) else Task.now(())
      val gcalInit = if(!gcalSim) initEpicsSystem(GcalEpics) else Task.now(())

      tcsInit *>
        gwsInit *>
        gcalInit *>
        instInit *>
        (for {
          now <- Task(LocalDate.now)
        } yield Settings(site,
                         odbHost,
                         now,
                         dhsServer,
                         dhsSim,
                         tcsSim,
                         instSim,
                         gcalSim,
                         odbNotifications,
                         tcsKeywords,
                         f2Keywords,
                         gwsKeywords,
                         instForceError)
        )

    }
  }

}
