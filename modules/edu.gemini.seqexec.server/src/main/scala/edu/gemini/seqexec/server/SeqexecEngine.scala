// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import java.time.LocalDate
import java.nio.file.Paths

import edu.gemini.epics.acm.CaService
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.spModel.core.Site
import edu.gemini.seqexec.engine
import edu.gemini.seqexec.engine.{Action, Engine, Event, EventSystem, Executed, Failed, Result, Sequence}
import edu.gemini.seqexec.engine.Result.{FileIdAllocated, Partial}
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.seqexec.odb.SmartGcal
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.model.Model.SeqexecEvent._
import edu.gemini.seqexec.model.{ActionType, UserDetails}
import edu.gemini.seqexec.server.flamingos2.{Flamingos2ControllerEpics, Flamingos2ControllerSim, Flamingos2ControllerSimBad, Flamingos2Epics}
import edu.gemini.seqexec.server.gcal.{GcalControllerEpics, GcalControllerSim, GcalEpics}
import edu.gemini.seqexec.server.gmos.{GmosControllerSim, GmosEpics, GmosNorthControllerEpics, GmosSouthControllerEpics}
import edu.gemini.seqexec.server.gws.GwsEpics
import edu.gemini.seqexec.server.tcs.{TcsControllerEpics, TcsControllerSim, TcsEpics}
import edu.gemini.spModel.core.Peer
import edu.gemini.spModel.seqcomp.SeqConfigNames.OCS_KEY
import edu.gemini.spModel.obscomp.InstConstants
import edu.gemini.spModel.core.SPProgramID
import knobs.Config

import scalaz._
import Scalaz._
import scalaz.concurrent.{Strategy, Task}
import scala.concurrent.duration._
import scalaz.stream.{DefaultScheduler, Process, wye}
import scalaz.stream.wye._
import scalaz.stream.time._

/**
  * Created by jluhrs on 10/7/16.
  */
class SeqexecEngine(settings: SeqexecEngine.Settings) {
  import SeqexecEngine._

  val odbProxy: ODBProxy = new ODBProxy(new Peer(settings.odbHost, 8443, null),
    if (settings.odbNotifications) ODBProxy.OdbCommandsImpl(new Peer(settings.odbHost, 8442, null))
    else ODBProxy.DummyOdbCommands)

  private val systems = SeqTranslate.Systems(
    odbProxy,
    if (settings.dhsSim) DhsClientSim(settings.date) else DhsClientHttp(settings.dhsURI),
    if (settings.tcsSim) TcsControllerSim else TcsControllerEpics,
    if (settings.gcalSim) GcalControllerSim else GcalControllerEpics,
    if (settings.instSim) {
      if (settings.instForceError) Flamingos2ControllerSimBad
      else Flamingos2ControllerSim
    } else Flamingos2ControllerEpics,
    if (settings.instSim) GmosControllerSim.south else GmosSouthControllerEpics,
    if (settings.instSim) GmosControllerSim.north else GmosNorthControllerEpics
  )

  private val translatorSettings = SeqTranslate.Settings(
    tcsKeywords = settings.tcsKeywords,
    f2Keywords = settings.f2Keywords,
    gwsKeywords = settings.gwsKeywords,
    gcalKeywords = settings.gcalKeywords,
    gmosKeywords = settings.gmosKeywords)

  private val translator = SeqTranslate(settings.site, systems, translatorSettings)

  def load(q: EventQueue, seqId: SPObservationID): Task[SeqexecFailure \/ Unit] = loadEvents(seqId).flatMapF(q.enqueueAll(_).map(_.right)).run

  def start(q: EventQueue, id: SPObservationID, user: UserDetails): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.start(id.stringValue(), user)).map(_.right)

  def requestPause(q: EventQueue, id: SPObservationID, user: UserDetails): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.pause(id.stringValue(), user)).map(_.right)

  def requestCancelPause(q: EventQueue, id: SPObservationID, user: UserDetails): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.cancelPause(id.stringValue(), user)).map(_.right)

  def setBreakpoint(q: EventQueue,
                    seqId: SPObservationID,
                    user: UserDetails,
                    stepId: edu.gemini.seqexec.engine.Step.Id,
                    v: Boolean): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.breakpoint(seqId.stringValue(), user, stepId, v)).map(_.right)

  def setOperator(q: EventQueue, user: UserDetails, name: Operator): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setOperator(name, user)).map(_.right)

  def setObserver(q: EventQueue,
                  seqId: SPObservationID,
                  user: UserDetails,
                  name: Observer): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setObserver(seqId.stringValue(), user, name)).map(_.right)

  def setConditions(q: EventQueue, conditions: Conditions, user: UserDetails): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setConditions(conditions, user)).map(_.right)

  def setImageQuality(q: EventQueue, iq: ImageQuality, user: UserDetails): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setImageQuality(iq, user)).map(_.right)

  def setWaterVapor(q: EventQueue, wv: WaterVapor, user: UserDetails): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setWaterVapor(wv, user)).map(_.right)

  def setSkyBackground(q: EventQueue, sb: SkyBackground, user: UserDetails): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setSkyBackground(sb, user)).map(_.right)

  def setCloudCover(q: EventQueue, cc: CloudCover, user: UserDetails): Task[SeqexecFailure \/ Unit] =
    q.enqueueOne(Event.setCloudCover(cc, user)).map(_.right)

  def setSkipMark: Task[SeqexecFailure \/ Unit] = ??? // scalastyle:ignore

  def requestRefresh(q: EventQueue): Task[Unit] = q.enqueueOne(Event.poll)

  def seqQueueRefreshProcess: Process[Task, Event] =
    awakeEvery(settings.odbQueuePollingInterval)(Strategy.DefaultStrategy, DefaultScheduler).map(_ => Event.getState(refreshSequenceList()))

  def eventProcess(q: EventQueue): Process[Task, SeqexecEvent] =
    engine.process(wye(q.dequeue, seqQueueRefreshProcess)(mergeHaltBoth))(Engine.State.empty).flatMap(x => Process.eval(notifyODB(x))).map {
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

  def stopObserve(q: EventQueue, seqId: SPObservationID): Task[Unit] = q.enqueueOne(
    Event.actionStop(seqId.stringValue, translator.stopObserve)
  )

  def abortObserve(q: EventQueue, seqId: SPObservationID): Task[Unit] = q.enqueueOne(
    Event.actionStop(seqId.stringValue, translator.abortObserve)
  )

  private def notifyODB(i: (Event, Engine.State)): Task[(Event, Engine.State)] = {
    def safeGetObsId(ids: String): SeqAction[SPObservationID] = EitherT(Task.delay(new SPObservationID(ids)).map(_.right).handle{
      case e: Exception => SeqexecFailure.SeqexecException(e).left
    })

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

  private def loadEvents(seqId: SPObservationID): SeqAction[List[Event]] = {
    val t: EitherT[Task, SeqexecFailure, (List[SeqexecFailure], Option[Sequence[Action \/ Result]])] = for {
      odbSeq       <- EitherT(Task.delay(odbProxy.read(seqId)))
      progIdString <- EitherT(Task.delay(odbSeq.config.extract(OCS_KEY / InstConstants.PROGRAMID_PROP).as[String].leftMap(ConfigUtilOps.explainExtractError)))
      progId       <- EitherT.fromTryCatchNonFatal(Task.now(SPProgramID.toProgramID(progIdString))).leftMap(e => SeqexecFailure.SeqexecException(e): SeqexecFailure)
    } yield translator.sequence(seqId, odbSeq)

    t.map {
      case (err :: _, None)  => List(Event.logMsg(SeqexecFailure.explain(err)))
      case (errs, Some(seq)) => Event.load(seqId.stringValue, seq) :: errs.map(e => Event.logMsg(SeqexecFailure.explain(e)))
      case _                 => Nil
    }
  }

  private def unloadEvent(seqId: SPObservationID): Event = Event.unload(seqId.stringValue)

  private def toSeqexecEvent(ev: engine.Event)(svs: => SequencesQueue[SequenceView]): SeqexecEvent = ev match {
    case engine.EventUser(ue) => ue match {
      case engine.Start(_, _)            => SequenceStart(svs)
      case engine.Pause(_, _)            => SequencePauseRequested(svs)
      case engine.CancelPause(_, _)      => SequencePauseCanceled(svs)
      case engine.Load(id, _)            => SequenceLoaded(id, svs)
      case engine.Unload(id)             => SequenceUnloaded(id, svs)
      case engine.Breakpoint(_, _, _, _) => StepBreakpointChanged(svs)
      case engine.SetOperator(_, _)      => OperatorUpdated(svs)
      case engine.SetObserver(_, _, _)   => ObserverUpdated(svs)
      case engine.SetConditions(_, _)    => ConditionsUpdated(svs)
      case engine.SetImageQuality(_, _)  => ConditionsUpdated(svs)
      case engine.SetWaterVapor(_, _)    => ConditionsUpdated(svs)
      case engine.SetSkyBackground(_, _) => ConditionsUpdated(svs)
      case engine.SetCloudCover(_, _)    => ConditionsUpdated(svs)
      case engine.Poll                   => SequenceRefreshed(svs)
      case engine.GetState(_)            => NullEvent
      case engine.ActionStop(_, _)       => ActionStopRequested(svs)
      case engine.Log(msg)               => NewLogMessage(msg)
    }
    case engine.EventSystem(se) => se match {
      // TODO: Sequence completed event not emited by engine.
      case engine.Completed(_, _, _)                                        => SequenceUpdated(svs)
      case engine.PartialResult(_, _, Partial(FileIdAllocated(fileId), _))  => FileIdStepExecuted(fileId, svs)
      case engine.PartialResult(_, _, _)                                    => SequenceUpdated(svs)
      case engine.Failed(_, _, _)                                           => NewLogMessage("Action failed")
      case engine.Busy(id)                                                  => ResourcesBusy(id, svs)
      case engine.Executed(_)                                               => StepExecuted(svs)
      case engine.Executing(_)                                              => SequenceUpdated(svs)
      case engine.Finished(_)                                               => SequenceCompleted(svs)
      case engine.Null                                                      => NullEvent
    }
  }


  def viewSequence(seq: SequenceAR, st: SequenceState): SequenceView = {

    def engineSteps(seq: SequenceAR): List[Step] = {

      // TODO: Calculate the whole status here and remove `Engine.Step.status`
      // This will be easier once the exact status labels in the UI are fixed.
      seq.steps.map(viewStep) match {
        // The sequence could be empty
        case Nil => Nil
        // Find first Pending Step when no Step is Running and mark it as Running
        case steps if (st === SequenceState.Running || st === SequenceState.Pausing || st === SequenceState.Stopping) && steps.all(_.status =/= StepState.Running) =>
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

  private def refreshSequenceList(): Engine.State => Task[Option[Process[Task, Event]]] = (st: Engine.State) => {

    val seqexecList = st.sequences.keys.toSeq.map(v => new SPObservationID(v))

    def loads(odbList: Seq[SPObservationID]): Task[List[Event]] =
      odbList.diff(seqexecList).toList.map(id => loadEvents(id)).sequenceU.map(_.flatten).run.map(_.valueOr( r => List(Event.logMsg(SeqexecFailure.explain(r)))))

    def unloads(odbList: Seq[SPObservationID]): Seq[Event] =
      seqexecList.diff(odbList).map(id => unloadEvent(id))

    val x = odbProxy.queuedSequences.flatMapF(seqs => loads(seqs).map(ee => (ee ++ unloads(seqs)).right)).run
    val y = x.map(_.valueOr(r => List(Event.logMsg(SeqexecFailure.explain(r)))))
    y.map { ee => ee.nonEmpty option Process.emitAll(ee).evalMap(Task.delay(_)) }
  }

}

// Configuration stuff
object SeqexecEngine {
  final case class Settings(site: Site,
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
  def apply(settings: Settings): SeqexecEngine = new SeqexecEngine(settings)

  val defaultSettings: Settings = Settings(Site.GS,
    "localhost",
    LocalDate.of(2017, 1, 1),
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

  // TODO: Better name and move it to `engine`
  type SequenceAR = engine.Sequence[engine.Action \/ engine.Result]
  type StepAR = engine.Step[engine.Action \/ engine.Result]

  // Couldn't find this on Scalaz
  def splitWhere[A](l: List[A])(p: (A => Boolean)): (List[A], List[A]) =
    l.splitAt(l.indexWhere(p))

  def splitAfter[A](l: List[A])(p: (A => Boolean)): (List[A], List[A]) =
    l.splitAt(l.indexWhere(p) + 1)

  private def kindToResource(kind: ActionType): List[Resource] = kind match {
    case ActionType.Configure(r) => List(r)
    case _                       => Nil
  }

  private[server] def configStatus(executions: List[List[engine.Action \/ engine.Result]]): List[(Resource, ActionStatus)] = {
    // Remove undefined actions
    val ex = executions.filter {
      case x => !x.rights.exists(_.kind === ActionType.Undefined)
    }
    // Split where at least one is running
    val (current, pending) = splitAfter(ex)(ex => ex.lefts.nonEmpty)

    // Calculate the state up to the current
    val configStatus = current.foldLeft(Map.empty[Resource, ActionStatus]) {
      case (s, e) =>
        val (a, r) = e.separate
          .bimap(
            _.flatMap(a => kindToResource(a.kind).strengthR(ActionStatus.Running)).toMap,
            _.flatMap(r => kindToResource(r.kind).strengthR(ActionStatus.Completed)).toMap)
        s ++ a ++ r
    }

    // Find out systems in the future
    val presentSystems = configStatus.keys.toList
    // Calculate status of pending items
    val systemsPending = pending.map {
      s => s.separate.bimap(_.map(_.kind).flatMap(kindToResource), _.map(_.kind).flatMap(kindToResource))
    }.flatMap {
      x => x._1.strengthR(ActionStatus.Pending) ::: x._2.strengthR(ActionStatus.Completed)
    }.filter {
      case (a, b) => !presentSystems.contains(a)
    }.distinct

    (configStatus ++ systemsPending).toList.sortBy(_._1)
  }

  /**
   * Calculates the config status for pending steps
   */
  private[server] def pendingConfigStatus(executions: List[List[engine.Action \/ engine.Result]]): List[(Resource, ActionStatus)] =
    executions.map {
      s => s.separate.bimap(_.map(_.kind).flatMap(kindToResource), _.map(_.kind).flatMap(kindToResource))
    }.flatMap {
      x => x._1 ::: x._2
    }.distinct.strengthR(ActionStatus.Pending).sortBy(_._1)

  /**
   * Overall pending status for a step
   */
  private def stepConfigStatus(step: StepAR): List[(Resource, ActionStatus)] =
    engine.Step.status(step) match {
      case StepState.Pending => pendingConfigStatus(step.executions)
      case _                 => configStatus(step.executions)
    }

  protected[server] def observeStatus(executions: List[List[engine.Action \/ engine.Result]], configStatus: List[(Resource, ActionStatus)]): ActionStatus = {
    def containsPartial(e: List[engine.Action \/ engine.Result]): Boolean =
      e.rights.exists {
        case Partial(FileIdAllocated(_), _) => true
        case _                              => false
      }

    def containsObserve(e: List[engine.Action \/ engine.Result]): Boolean =
      e.rights.map(_.kind).contains(ActionType.Observe)

    if (configStatus.forall(_._2 === ActionStatus.Completed)) {
      // Find one with kind observe
      executions.filter { e =>
        val (a, r) = e.separate.bimap(_.map(_.kind), _.map(_.kind))
        a.contains(ActionType.Observe) || r.contains(ActionType.Observe)
      }.map {
        case e if containsPartial(e) => ActionStatus.Running
        case e if containsObserve(e) => ActionStatus.Completed
        case _                       => ActionStatus.Running
      }.headOption.getOrElse(ActionStatus.Pending)
    } else ActionStatus.Pending
  }

  def viewStep(step: StepAR): StandardStep = {
    val configStatus = stepConfigStatus(step)
    StandardStep(
      id = step.id,
      config = step.config,
      status = engine.Step.status(step),
      // TODO: Implement breakpoints at Engine level
      breakpoint = step.breakpoint,
      // TODO: Implement skipping at Engine level
      skip = false,
      configStatus = configStatus,
      observeStatus = observeStatus(step.executions, configStatus),
      fileId = step.fileId
    )
  }

  private def decodeTops(s: String): Map[String, String] =
    s.split("=|,").grouped(2).collect {
      case Array(k, v) => k.trim -> v.trim
    }.toMap

  private def initSmartGCal(smartGCalHost: String, smartGCalLocation: String): Task[edu.gemini.seqexec.odb.TrySeq[Unit]] = {
    // SmartGCal always talks to GS
    val peer = new Peer(smartGCalHost, 8443, Site.GS)
    Task.delay(Paths.get(smartGCalLocation)).map { p => SmartGcal.initialize(peer, p) }
  }

  private val taskUnit = Task.now(())

  // scalastyle:off
  def seqexecConfiguration: Kleisli[Task, Config, Settings] = Kleisli { cfg: Config => {
    val site = cfg.require[String]("seqexec-engine.site") match {
      case "GS" => Site.GS
      case "GN" => Site.GN
      case _    => Site.GS // Let's default to GS
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
    val smartGCalHost           = cfg.require[String]("seqexec-engine.smartGCalHost")
    val smartGCalDir            = cfg.require[String]("seqexec-engine.smartGCalDir")

    // TODO: Review initialization of EPICS systems
    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def initEpicsSystem[T](sys: EpicsSystem[T], tops: Map[String, String]): Task[Unit] =
      Task.delay(
        Option(CaService.getInstance()) match {
          case None => throw new Exception("Unable to start EPICS service.")
          case Some(s) =>
            sys.init(s, tops).leftMap {
                case SeqexecFailure.SeqexecException(ex) => throw ex
                case c: SeqexecFailure                   => throw new Exception(SeqexecFailure.explain(c))
            }
        }
      ) *> taskUnit

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
    val gcalInit = if (gcalKeywords || !gcalSim) initEpicsSystem(GcalEpics, tops) else taskUnit

    initSmartGCal(smartGCalHost, smartGCalDir) *>
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
  // scalastyle:on

}
