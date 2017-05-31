package edu.gemini.seqexec.server

import java.time.LocalDate

import edu.gemini.epics.acm.CaService
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.model.p1.immutable.Site
import edu.gemini.seqexec.engine
import edu.gemini.seqexec.engine.{Action, Engine, Event, EventSystem, Executed, Failed, Sequence}
import edu.gemini.seqexec.server.ConfigUtilOps._

import scalaz._
import Scalaz._
import scalaz.concurrent.{Strategy, Task}
import scala.concurrent.duration._
import scalaz.stream.{DefaultScheduler, Process, wye}
import scalaz.stream.wye._
import scalaz.stream.time._
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.model.Model.SeqexecEvent._
import edu.gemini.spModel.core.Peer
import edu.gemini.spModel.seqcomp.SeqConfigNames.OCS_KEY
import edu.gemini.spModel.obscomp.InstConstants
import edu.gemini.spModel.core.SPProgramID
import knobs.Config

/**
  * Created by jluhrs on 10/7/16.
  */
class SeqexecEngine(settings: SeqexecEngine.Settings) {

  val odbProxy = new ODBProxy(new Peer(settings.odbHost, 8443, null),
    if (settings.odbNotifications) ODBProxy.OdbCommandsImpl(new Peer(settings.odbHost, 8442, null))
    else ODBProxy.DummyOdbCommands)

  val odbClient = ODBClient(ODBClientConfig(settings.odbHost, ODBClient.DefaultODBBrowserPort))

  val systems = SeqTranslate.Systems(
    odbProxy,
    if (settings.dhsSim) DhsClientSim(settings.date) else DhsClientHttp(settings.dhsURI),
    if (settings.tcsSim) TcsControllerSim else TcsControllerEpics,
    if (settings.gcalSim) GcalControllerSim else GcalControllerEpics,
    if (settings.instSim) {
      if (settings.instForceError) Flamingos2ControllerSimBad
      else Flamingos2ControllerSim
    } else Flamingos2ControllerEpics,
    if (settings.instSim) GmosSouthControllerSim else GmosControllerEpics
  )

  val translatorSettings = SeqTranslate.Settings(
    tcsKeywords = settings.tcsKeywords,
    f2Keywords = settings.f2Keywords,
    gwsKeywords = settings.gwsKeywords,
    gcalKeywords = settings.gcalKeywords,
    gmosKeywords = settings.gmosKeywords)

  val translator = SeqTranslate(settings.site, systems, translatorSettings)

  def start(q: engine.EventQueue, id: SPObservationID): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.start(id.stringValue())).map(_.right)

  def requestPause(q: engine.EventQueue, id: SPObservationID): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.pause(id.stringValue())).map(_.right)

  def setBreakpoint(q: engine.EventQueue,
                    seqId: SPObservationID,
                    stepId: edu.gemini.seqexec.engine.Step.Id,
                    v: Boolean): Task[SeqexecFailure \/ Unit] =
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

  def setSkipMark(q: engine.EventQueue, id: SPObservationID, stepId: edu.gemini.seqexec.engine.Step.Id): Task[SeqexecFailure \/ Unit] = ???

  def requestRefresh(q: engine.EventQueue): Task[Unit] = q.enqueueOne(Event.poll)

  def seqQueueRefreshProcess(q: engine.EventQueue): Process[Task, Event] = awakeEvery(settings.odbQueuePollingInterval)(Strategy.DefaultStrategy, DefaultScheduler).map(_ => Event.getState(refreshSequenceList(q)))

  def eventProcess(q: engine.EventQueue): Process[Task, SeqexecEvent] =
    engine.process(q, wye(q.dequeue, seqQueueRefreshProcess(q))(mergeHaltBoth))(Engine.State.empty).flatMap(x => Process.eval(notifyODB(x))).map {
      case (ev, qState) =>
        toSeqexecEvent(ev)(
          SequencesQueue(
            qState.conditions,
            qState.operator,
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
        _ <- systems.odb.obsAbort(obsId, e.msg)
      } yield ()
      case (EventSystem(Executed(id)), st) if st.sequences.get(id).exists(_.status === SequenceState.Idle) => for {
        obsId <- safeGetObsId(id)
        _ <- systems.odb.obsPause(obsId, "Sequence paused by user")
      } yield ()
      case _ => SeqAction(())
    }).run.map(_ => i)
  }

  def load(q: engine.EventQueue, seqId: SPObservationID): Task[SeqexecFailure \/ Unit] = {
    val t: EitherT[Task, SeqexecFailure, (List[SeqexecFailure], Option[Sequence[Action]])] = for {
      odbSeq       <- EitherT(Task(odbProxy.read(seqId)))
      progIdString <- EitherT(Task.delay(odbSeq.extract(OCS_KEY / InstConstants.PROGRAMID_PROP).as[String].leftMap(ConfigUtilOps.explainExtractError)))
      progId       <- EitherT.fromTryCatchNonFatal(Task.now(SPProgramID.toProgramID(progIdString))).leftMap(e => SeqexecFailure.SeqexecException(e): SeqexecFailure)
      name         <- EitherT(odbClient.observationTitle(progId, seqId.toString).map(_.leftMap(ConfigUtilOps.explainExtractError)))
    } yield translator.sequence(translatorSettings)(seqId, odbSeq, name)

    val u = t.flatMapF{
      case (err :: _, None)  => q.enqueueOne(Event.logMsg(SeqexecFailure.explain(err))).map(_.right)
      case (errs, Some(seq)) =>
        (if(errs.isEmpty) Task(()) else q.enqueueAll(errs.map(e => Event.logMsg(SeqexecFailure.explain(e))))) *> Task.delay {
          q.enqueueOne(Event.load(seqId.stringValue(), seq)).unsafePerformAsync(x => ()).right[SeqexecFailure]
        }
      case _                 => Task(().right)
    }
    u.run
  }

  def unload(q: engine.EventQueue, seqId: SPObservationID): Task[SeqexecFailure \/ Unit] = {
    q.enqueueOne(Event.unload(seqId.stringValue)).map(_.right)
  }

  private def toSeqexecEvent(ev: engine.Event)(svs: SequencesQueue[SequenceView]): SeqexecEvent = ev match {
    case engine.EventUser(ue) => ue match {
      case engine.Start(_)            => SequenceStart(svs)
      case engine.Pause(_)            => SequencePauseRequested(svs)
      case engine.Load(id, _)         => SequenceLoaded(id, svs)
      case engine.Unload(id)          => SequenceUnloaded(id, svs)
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
      case engine.GetState(_)         => NewLogMessage("Internal state request")
      case engine.Log(msg)            => NewLogMessage(msg)
    }
    case engine.EventSystem(se) => se match {
      // TODO: Sequence completed event not emited by engine.
      case engine.Completed(_, _, _)     => NewLogMessage("Action completed")
      case engine.PartialResult(_, _, _) => SequenceUpdated(svs)
      case engine.Failed(_, _, _)        => NewLogMessage("Action failed")
      case engine.Busy(_)                => ResourcesBusy(svs)
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

  private def refreshSequenceList(q: engine.EventQueue): Engine.State => Task[Unit] = (st: Engine.State) => {

    val seqexecList = st.sequences.keys.toSeq.map(v => new SPObservationID(v))

    def loads(odbList: Seq[SPObservationID]): Seq[Task[SeqexecFailure \/ Unit]] = odbList.diff(seqexecList).map(id => load(q, id))

    def unloads(odbList: Seq[SPObservationID]): Seq[Task[SeqexecFailure \/ Unit]] = seqexecList.diff(odbList).map(id => unload(q, id))

    odbProxy.queuedSequences().flatMap(seqs => EitherT(Nondeterminism[Task].gatherUnordered(loads(seqs) ++
      unloads(seqs)).map(_.sequenceU))
    ).run.map(_ => ())
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
                      gmosKeywords: Boolean,
                      gwsKeywords: Boolean,
                      gcalKeywords: Boolean,
                      instForceError: Boolean,
                      odbQueuePollingInterval: Duration)
  val defaultSettings = Settings(Site.GS,
    "localhost",
    LocalDate.of(2017, 1,1),
    "http://localhost/",
    dhsSim = true,
    tcsSim = true,
    instSim = true,
    gcalSim = true,
    odbNotifications = false,
    tcsKeywords = false,
    f2Keywords = false,
    gmosKeywords = false,
    gwsKeywords = false,
    gcalKeywords = false,
    instForceError = false,
    10.seconds)

  def apply(settings: Settings) = new SeqexecEngine(settings)


  private def decodeTops(s: String): Map[String, String] = s.split("=|,").grouped(2).map { case Array(k, v) => k.trim -> v.trim }.toMap

  def seqexecConfiguration: Kleisli[Task, Config, Settings] = Kleisli { cfg: Config => {
    val site = cfg.require[String]("seqexec-engine.site") match {
      case "GS" => Site.GS
      case "GN" => Site.GN
    }
    val odbHost                 = cfg.require[String]("seqexec-engine.odb")
    val dhsServer               = cfg.require[String]("seqexec-engine.dhsServer")
    val dhsSim                  = cfg.require[Boolean]("seqexec-engine.dhsSim")
    val tcsSim                  = cfg.require[Boolean]("seqexec-engine.tcsSim")
    val instSim                 = cfg.require[Boolean]("seqexec-engine.instSim")
    val gcalSim                 = cfg.require[Boolean]("seqexec-engine.gcalSim")
    val odbNotifications        = cfg.require[Boolean]("seqexec-engine.odbNotifications")
    val tcsKeywords             = cfg.require[Boolean]("seqexec-engine.tcsKeywords")
    val f2Keywords              = cfg.require[Boolean]("seqexec-engine.f2Keywords")
    val gmosKeywords            = cfg.require[Boolean]("seqexec-engine.gmosKeywords")
    val gwsKeywords             = cfg.require[Boolean]("seqexec-engine.gwsKeywords")
    val gcalKeywords            = cfg.require[Boolean]("seqexec-engine.gcalKeywords")
    val instForceError          = cfg.require[Boolean]("seqexec-engine.instForceError")
    val odbQueuePollingInterval = Duration(cfg.require[String]("seqexec-engine.odbQueuePollingInterval"))
    val tops                    = decodeTops(cfg.require[String]("seqexec-engine.tops"))
    val caAddrList              = cfg.lookup[String]("seqexec-engine.epics_ca_addr_list")

    // TODO: Review initialization of EPICS systems
    def initEpicsSystem[T](sys: EpicsSystem[T], tops: Map[String, String]): Task[Unit] =
      Task.delay(
        Option(CaService.getInstance()) match {
          case None => throw new Exception("Unable to start EPICS service.")
          case Some(s) =>
            sys.init(s, tops).leftMap {
                case SeqexecFailure.SeqexecException(ex) => throw ex
                case c: SeqexecFailure => throw new Exception(SeqexecFailure.explain(c))
            }
        }
      )

    val taskUnit = Task.now(())
    // Ensure there is a valid way to init CaService either from
    // the configuration file or from the environment
    val caInit   = caAddrList.map(a => Task.delay(CaService.setAddressList(a))).getOrElse {
      Task.delay(Option(System.getenv("EPICS_CA_ADDR_LIST"))).flatMap {
        case Some(_) => taskUnit // Do nothing, just check that it exists
        case _       => Task.fail(new RuntimeException("Cannot initialize EPICS subsystem"))
      }
    }
    val tcsInit  = if (tcsKeywords || !tcsSim) initEpicsSystem(TcsEpics, tops) else taskUnit
    // More instruments to be added to the list here
    val instInit = Nondeterminism[Task].gatherUnordered(List((f2Keywords, Flamingos2Epics), (gmosKeywords, GmosEpics)).filter(_._1 || !instSim).map(x => initEpicsSystem(x._2, tops)))
    val gwsInit  = if (gwsKeywords) initEpicsSystem(GwsEpics, tops) else taskUnit
    val gcalInit = if (!gcalSim) initEpicsSystem(GcalEpics, tops) else taskUnit

    caInit *>
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
                       gmosKeywords,
                       gwsKeywords,
                       gcalKeywords,
                       instForceError,
                       odbQueuePollingInterval)
      )

    }
  }

}
