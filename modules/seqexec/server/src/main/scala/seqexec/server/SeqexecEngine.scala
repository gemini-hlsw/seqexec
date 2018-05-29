// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import java.nio.file.Paths
import java.time.LocalDate
import java.util.concurrent.TimeUnit

import cats._
import cats.data.{EitherT, Kleisli}
import cats.effect.IO
import cats.implicits._
import edu.gemini.epics.acm.CaService
import edu.gemini.pot.sp.SPObservationID
import seqexec.engine
import seqexec.engine.Result.{FileIdAllocated, Partial}
import seqexec.engine.{Step => _, _}
import seqexec.model.Model._
import seqexec.model.events._
import seqexec.model.{ActionType, UserDetails}
import seqexec.server.ConfigUtilOps._
import seqexec.server.EngineMetadata.queuesL
import seqexec.server.flamingos2.{Flamingos2ControllerEpics, Flamingos2ControllerSim, Flamingos2ControllerSimBad, Flamingos2Epics}
import seqexec.server.gcal.{GcalControllerEpics, GcalControllerSim, GcalEpics}
import seqexec.server.gmos.{GmosControllerSim, GmosEpics, GmosNorthControllerEpics, GmosSouthControllerEpics}
import seqexec.server.gnirs.{GnirsControllerEpics, GnirsControllerSim, GnirsEpics}
import seqexec.server.gws.GwsEpics
import seqexec.server.tcs.{TcsControllerEpics, TcsControllerSim, TcsEpics}
import edu.gemini.seqexec.odb.SmartGcal
import edu.gemini.spModel.core.{Peer, SPProgramID, Site}
import edu.gemini.spModel.obscomp.InstConstants
import edu.gemini.spModel.seqcomp.SeqConfigNames.OCS_KEY
import fs2.{Scheduler, Stream}
import knobs.Config
import mouse.all._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

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
      if (settings.instForceError) Flamingos2ControllerSimBad(settings.failAt)
      else Flamingos2ControllerSim
    } else Flamingos2ControllerEpics,
    if (settings.instSim) GmosControllerSim.south else GmosSouthControllerEpics,
    if (settings.instSim) GmosControllerSim.north else GmosNorthControllerEpics,
    if (settings.instSim) GnirsControllerSim else GnirsControllerEpics
  )

  private val translatorSettings = SeqTranslate.Settings(
    tcsKeywords = settings.tcsKeywords,
    f2Keywords = settings.f2Keywords,
    gwsKeywords = settings.gwsKeywords,
    gcalKeywords = settings.gcalKeywords,
    gmosKeywords = settings.gmosKeywords,
    gnirsKeywords = settings.gnirsKeywords
  )

  private val translator = SeqTranslate(settings.site, systems, translatorSettings)

  def load(q: EventQueue, seqId: SPObservationID): IO[Either[SeqexecFailure, Unit]] =
    loadEvents(seqId).flatMapF(b => q.enqueue(Stream.emits(b)).map(_.asRight).compile.last.attempt.map(_.bimap(SeqexecFailure.SeqexecException.apply, _ => ()))).value

  def start(q: EventQueue, id: SPObservationID, user: UserDetails, clientId: ClientID): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.start(id.stringValue(), user, clientId)).map(_.asRight)

  def requestPause(q: EventQueue, id: SPObservationID, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.pause(id.stringValue(), user)).map(_.asRight)

  def requestCancelPause(q: EventQueue, id: SPObservationID, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.cancelPause(id.stringValue(), user)).map(_.asRight)

  def setBreakpoint(q: EventQueue,
                    seqId: SPObservationID,
                    user: UserDetails,
                    stepId: seqexec.engine.Step.Id,
                    v: Boolean): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.breakpoint(seqId.stringValue(), user, stepId, v)).map(_.asRight)

  def setOperator(q: EventQueue, user: UserDetails, name: Operator): IO[Either[SeqexecFailure, Unit]] =
     q.enqueue1(Event.logDebugMsg(s"SeqexecEngine: Setting Operator name to '$name' by ${user.username}")) *>
     q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((Engine.State.userDataL ^|-> EngineMetadata.operatorL).set(name.some), SetOperator(name, user.some))).map(_.asRight)

  def setObserver(q: EventQueue,
                  seqId: SPObservationID,
                  user: UserDetails,
                  name: Observer): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.setObserver(seqId.stringValue(), user, name)).map(_.asRight)

  def setConditions(q: EventQueue, conditions: Conditions, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting conditions")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((Engine.State.userDataL ^|-> EngineMetadata.conditionsL).set(conditions), SetConditions(conditions, user.some))).map(_.asRight)

  def setImageQuality(q: EventQueue, iq: ImageQuality, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting image quality")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((Engine.State.userDataL ^|-> EngineMetadata.conditionsL ^|-> Conditions.iq).set(iq), SetImageQuality(iq, user.some))).map(_.asRight)

  def setWaterVapor(q: EventQueue, wv: WaterVapor, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting water vapor")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((Engine.State.userDataL ^|-> EngineMetadata.conditionsL ^|-> Conditions.wv).set(wv), SetWaterVapor(wv, user.some))).map(_.asRight)

  def setSkyBackground(q: EventQueue, sb: SkyBackground, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting sky background")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((Engine.State.userDataL ^|-> EngineMetadata.conditionsL ^|-> Conditions.sb).set(sb), SetSkyBackground(sb, user.some))).map(_.asRight)

  def setCloudCover(q: EventQueue, cc: CloudCover, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting cloud cover")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((Engine.State.userDataL ^|-> EngineMetadata.conditionsL ^|-> Conditions.cc).set(cc), SetCloudCover(cc, user.some))).map(_.asRight)

  def setSkipMark(q: EventQueue,
                  seqId: SPObservationID,
                  user: UserDetails,
                  stepId: seqexec.engine.Step.Id,
                  v: Boolean): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.skip(seqId.stringValue(), user, stepId, v)).map(_.asRight)

  def requestRefresh(q: EventQueue, clientId: ClientID): IO[Unit] = q.enqueue1(Event.poll(clientId))

  def seqQueueRefreshStream: Stream[IO, executeEngine.EventType] =
    Scheduler[IO](corePoolSize = 1).flatMap { scheduler =>
      val fd = Duration(settings.odbQueuePollingInterval.toSeconds, TimeUnit.SECONDS)
      scheduler.fixedRate[IO](fd).flatMap { _ =>
        Stream.emit(Event.getState[executeEngine.ConcreteTypes](refreshSequenceList()))
      }
    }

  def eventStream(q: EventQueue): Stream[IO, SeqexecEvent] = {
    executeEngine.process(q.dequeue.mergeHaltBoth(seqQueueRefreshStream))(Engine.State.empty[EngineMetadata](EngineMetadata.default)).flatMap(x => Stream.eval(notifyODB(x))).map {
      case (ev, qState) =>
        toSeqexecEvent(ev)(
          SequencesQueue(
            (Engine.State.userDataL ^|-> EngineMetadata.conditionsL).get(qState),
            (Engine.State.userDataL ^|-> EngineMetadata.operatorL).get(qState),
            qState.sequences.values.map(
              s => viewSequence(s.toSequence, s)
            ).toList
          )
        )
    }
  }

  def stopObserve(q: EventQueue, seqId: SPObservationID): IO[Unit] = q.enqueue1(
    Event.actionStop[executeEngine.ConcreteTypes](seqId.stringValue, translator.stopObserve(seqId.stringValue))
  )

  def abortObserve(q: EventQueue, seqId: SPObservationID): IO[Unit] = q.enqueue1(
    Event.actionStop[executeEngine.ConcreteTypes](seqId.stringValue, translator.abortObserve(seqId.stringValue))
  )

  def pauseObserve(q: EventQueue, seqId: SPObservationID): IO[Unit] = q.enqueue1(
    Event.actionStop[executeEngine.ConcreteTypes](seqId.stringValue, translator.pauseObserve)
  )

  def resumeObserve(q: EventQueue, seqId: SPObservationID): IO[Unit] = q.enqueue1(
    Event.getSeqState[executeEngine.ConcreteTypes](seqId.stringValue, translator.resumePaused(seqId.stringValue))
  )

  private def addSeq(name: String, seqId: Sequence.Id)(st: executeEngine.StateType): executeEngine.StateType = (
    for {
      q   <- st.userData.queues.get(name)
      seq <- st.sequences.get(seqId)
    } yield if(!q.status(st).isRunning && !seq.status.isRunning && !seq.status.isCompleted && !q.contains(seqId)) st.copy(userData = queuesL.modify(_.updated(name, q :+ seqId))(st.userData)) else st
  ).getOrElse(st)

  def addSequenceToQueue(q: EventQueue, queueName: String, seqId: SPObservationID): IO[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[executeEngine.ConcreteTypes](addSeq(queueName, seqId.stringValue), NullSeqEvent)
  ).map(_.asRight)

  private def removeSeq(name: String, seqId: Sequence.Id)(st: executeEngine.StateType): executeEngine.StateType = (
    for {
      q <- st.userData.queues.get(name)
    } yield if(!q.status(st).isRunning && q.contains(seqId)) st.copy(userData = queuesL.modify(_.updated(name, q.filterNot(_===seqId)))(st.userData)) else st
  ).getOrElse(st)

  def removeSequenceFromQueue(q: EventQueue, queueName: String, seqId: SPObservationID): IO[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[executeEngine.ConcreteTypes](removeSeq(queueName, seqId.stringValue), NullSeqEvent)
  ).map(_.asRight)

  // This assumes that there is only one instance of e in l
  private def moveElement[T](l: List[T], e: T, d: Int)(implicit eq: Eq[T]): List[T] = {
    val idx = l.indexOf(e)

    if(d === 0 || idx<0) l
    else {
      val (h, t) = l.filterNot(_ === e).splitAt(idx+d)
      (h :+ e) ::: t
    }
  }

  private def moveSeq(name: String, seqId: Sequence.Id, d: Int)(st: executeEngine.StateType): executeEngine.StateType = (
    for {
      q <- st.userData.queues.get(name)
    } yield if(!q.status(st).isRunning && q.contains(seqId)) st.copy(userData = queuesL.modify(_.updated(name, moveElement(q, seqId, d)))(st.userData)) else st
  ).getOrElse(st)

  def moveSequenceInQueue(q: EventQueue, queueName: String, seqId: SPObservationID, d: Int): IO[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[executeEngine.ConcreteTypes](moveSeq(queueName, seqId.stringValue, d), NullSeqEvent)
  ).map(_.asRight)

  def notifyODB(i: (executeEngine.EventType, executeEngine.StateType)): IO[(executeEngine.EventType, executeEngine.StateType)] = {
    def safeGetObsId(ids: String): SeqAction[SPObservationID] = EitherT(IO.apply(new SPObservationID(ids)).attempt.map { _.leftMap(SeqexecFailure.SeqexecException.apply) })

    (i match {
      case (EventSystem(Failed(id, _, e)), _) => safeGetObsId(id) >>= {systems.odb.obsAbort(_, e.msg)}
      case (EventSystem(Executed(id)), st) if st.sequences.get(id).exists(_.status === SequenceState.Idle) =>
        safeGetObsId(id) >>= {systems.odb.obsPause(_, "Sequence paused by user")}
      case (EventSystem(Finished(id)), _)     => safeGetObsId(id) >>= systems.odb.sequenceEnd
      case _                                  => SeqAction(())
    }).value.map(_ => i)
  }

  private def loadEvents(seqId: SPObservationID): SeqAction[List[executeEngine.EventType]] = {
    val t: EitherT[IO, SeqexecFailure, (List[SeqexecFailure], Option[Sequence])] = for {
      odbSeq       <- SeqAction.either(odbProxy.read(seqId))
      progIdString <- SeqAction.either(odbSeq.config.extract(OCS_KEY / InstConstants.PROGRAMID_PROP).as[String].leftMap(ConfigUtilOps.explainExtractError))
      _            <- SeqAction.either(Either.catchNonFatal(IO.pure(SPProgramID.toProgramID(progIdString))).leftMap(e => SeqexecFailure.SeqexecException(e): SeqexecFailure))
    } yield translator.sequence(seqId, odbSeq)

    t.map {
      case (err :: _, None)  => List(Event.logDebugMsg(SeqexecFailure.explain(err)))
      case (errs, Some(seq)) => Event.load(seqId.stringValue, seq) :: errs.map(e => Event.logDebugMsg(SeqexecFailure.explain(e)))
      case _                 => Nil
    }
  }

  private def modifyStateEvent(v: SeqEvent, svs: => SequencesQueue[SequenceView]): SeqexecEvent = v match {
    case NullSeqEvent           => NullEvent
    case SetOperator(_, _)      => OperatorUpdated(svs)
    case SetObserver(_, _, _)   => ObserverUpdated(svs)
    case SetConditions(_, _)    => ConditionsUpdated(svs)
    case SetImageQuality(_, _)  => ConditionsUpdated(svs)
    case SetWaterVapor(_, _)    => ConditionsUpdated(svs)
    case SetSkyBackground(_, _) => ConditionsUpdated(svs)
    case SetCloudCover(_, _)    => ConditionsUpdated(svs)
  }

  def toSeqexecEvent(ev: executeEngine.EventType)(svs: => SequencesQueue[SequenceView]): SeqexecEvent = ev match {
    case engine.EventUser(ue) => ue match {
      case engine.Start(_, _, _)         => SequenceStart(svs)
      case engine.Pause(_, _)            => SequencePauseRequested(svs)
      case engine.CancelPause(_, _)      => SequencePauseCanceled(svs)
      case engine.Load(id, _)            => SequenceLoaded(id, svs)
      case engine.Unload(id)             => SequenceUnloaded(id, svs)
      case engine.Breakpoint(_, _, _, _) => StepBreakpointChanged(svs)
      case engine.SkipMark(_, _, _, _)   => StepSkipMarkChanged(svs)
      case engine.SetObserver(_, _, _)   => ObserverUpdated(svs)
      case engine.Poll(cid)              => SequenceRefreshed(svs, cid)
      case engine.GetState(_)            => NullEvent
      case engine.GetSeqState(_, _)      => NullEvent
      case engine.ModifyState(_, ev)     => modifyStateEvent(ev, svs)
      case engine.ActionStop(_, _)       => ActionStopRequested(svs)
      case engine.LogDebug(_)            => NullEvent
      case engine.LogInfo(_)             => NullEvent
      case engine.LogWarning(_)          => NullEvent
      case engine.LogError(_)            => NullEvent
      case engine.ActionResume(_, _, _)  => SequenceUpdated(svs)
    }
    case engine.EventSystem(se) => se match {
      // TODO: Sequence completed event not emited by engine.
      case engine.Completed(_, _, _)                                        => SequenceUpdated(svs)
      case engine.PartialResult(_, _, Partial(FileIdAllocated(fileId), _))  => FileIdStepExecuted(fileId, svs)
      case engine.PartialResult(_, _, _)                                    => SequenceUpdated(svs)
      case engine.Failed(id, _, _)                                          => SequenceError(id, svs)
      case engine.Busy(id, clientId)                                        => ResourcesBusy(id, svs, clientId)
      case engine.Executed(s)                                               => StepExecuted(s, svs)
      case engine.Executing(_)                                              => SequenceUpdated(svs)
      case engine.Finished(_)                                               => SequenceCompleted(svs)
      case engine.Null                                                      => NullEvent
      case engine.Paused(id, _, _)                                          => ExposurePaused(id, svs)
      case engine.BreakpointReached(id)                                     => SequencePaused(id, svs)
    }
  }

  def viewSequence(seq: Sequence, st: Sequence.State): SequenceView = {

    def engineSteps(seq: Sequence): List[Step] = {

      // TODO: Calculate the whole status here and remove `Engine.Step.status`
      // This will be easier once the exact status labels in the UI are fixed.
      seq.steps.map(viewStep) match {
        // The sequence could be empty
        case Nil => Nil
        // Find first Pending Step when no Step is Running and mark it as Running
        case steps if Sequence.State.isRunning(st) && steps.forall(_.status =!= StepState.Running) =>
          val (xs, (y :: ys)) = splitWhere(steps)(_.status === StepState.Pending)
          xs ++ (y.copy(status = StepState.Running) :: ys)
        case steps if st.status === SequenceState.Idle && steps.exists(_.status === StepState.Running) =>
          val (xs, (y :: ys)) = splitWhere(steps)(_.status === StepState.Running)
          xs ++ (y.copy(status = StepState.Paused) :: ys)
        case x => x
      }
    }

    // TODO: Implement willStopIn
    SequenceView(seq.id, seq.metadata, st.status, engineSteps(seq), None)
  }

   private def unloadEvent(seqId: SPObservationID): executeEngine.EventType = Event.unload(seqId.stringValue)

  private def refreshSequenceList(): executeEngine.StateType => IO[Option[Stream[IO, executeEngine.EventType]]] = (st: executeEngine.StateType) => {
    val seqexecList = st.sequences.keys.toSeq.map(v => new SPObservationID(v))

    def loads(odbList: Seq[SPObservationID]): IO[List[executeEngine.EventType]] =
      odbList.diff(seqexecList).toList.map(id => loadEvents(id)).sequence.map(_.flatten).value.map(_.valueOr(r => List(Event.logDebugMsg(SeqexecFailure.explain(r)))))

    def unloads(odbList: Seq[SPObservationID]): Seq[executeEngine.EventType] =
      seqexecList.diff(odbList).map(id => unloadEvent(id))

    val x = odbProxy.queuedSequences.flatMapF(seqs => loads(seqs).map(ee => (ee ++ unloads(seqs)).asRight)).value
    val y = x.map(_.valueOr(r => List(Event.logWarningMsg(SeqexecFailure.explain(r)))))
    y.map { ee => ee.nonEmpty option Stream.emits(ee).evalMap(IO.apply(_)) }
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
                      gnirsKeywords: Boolean,
                      instForceError: Boolean,
                      failAt: Int,
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
    gnirsKeywords = false,
    instForceError = false,
    failAt = 0,
    10.seconds)

  // Couldn't find this on Scalaz
  def splitWhere[A](l: List[A])(p: (A => Boolean)): (List[A], List[A]) =
    l.splitAt(l.indexWhere(p))

  def splitAfter[A](l: List[A])(p: (A => Boolean)): (List[A], List[A]) =
    l.splitAt(l.indexWhere(p) + 1)

  private[server] def actionStateToStatus(s: engine.Action.ActionState): ActionStatus = s match {
    case engine.Action.Idle                  => ActionStatus.Pending
    case engine.Action.Completed(_)          => ActionStatus.Completed
    case engine.Action.Started               => ActionStatus.Running
    case engine.Action.Paused(_)             => ActionStatus.Paused
    case engine.Action.Failed(_)             => ActionStatus.Failed
  }

  private def kindToResource(kind: ActionType): List[Resource] = kind match {
    case ActionType.Configure(r) => List(r)
    case _                       => Nil
  }

  private[server] def separateActions(ls: List[Action]): (List[Action], List[Action]) =  ls.partition{ _.state.runState match {
    case engine.Action.Completed(_) => false
    case engine.Action.Failed(_)    => false
    case _                          => true
  } }

  private[server] def configStatus(executions: List[List[engine.Action]]): List[(Resource, ActionStatus)] = {
    // Remove undefined actions
    val ex = executions.filter { !separateActions(_)._2.exists(_.kind === ActionType.Undefined) }
    // Split where at least one is running
    val (current, pending) = splitAfter(ex)(separateActions(_)._1.nonEmpty)

    // Calculate the state up to the current
    val configStatus = current.foldLeft(Map.empty[Resource, ActionStatus]) {
      case (s, e) =>
        val (a, r) = separateActions(e).bimap(
            _.flatMap(a => kindToResource(a.kind).tupleRight(ActionStatus.Running)).toMap,
            _.flatMap(r => kindToResource(r.kind).tupleRight(ActionStatus.Completed)).toMap)
        s ++ a ++ r
    }

    // Find out systems in the future
    val presentSystems = configStatus.keys.toList
    // Calculate status of pending items
    val systemsPending = pending.map {
      s => separateActions(s).bimap(_.map(_.kind).flatMap(kindToResource), _.map(_.kind).flatMap(kindToResource))
    }.flatMap {
      x => x._1.tupleRight(ActionStatus.Pending) ::: x._2.tupleRight(ActionStatus.Completed)
    }.filter {
      case (a, _) => !presentSystems.contains(a)
    }.distinct

    (configStatus ++ systemsPending).toList.sortBy(_._1)
  }

  /**
   * Calculates the config status for pending steps
   */
  private[server] def pendingConfigStatus(executions: List[List[engine.Action]]): List[(Resource, ActionStatus)] =
    executions.map {
      s => separateActions(s).bimap(_.map(_.kind).flatMap(kindToResource), _.map(_.kind).flatMap(kindToResource))
    }.flatMap {
      x => x._1 ::: x._2
    }.distinct.tupleRight(ActionStatus.Pending).sortBy(_._1)

  /**
   * Overall pending status for a step
   */
  private def stepConfigStatus(step: engine.Step): List[(Resource, ActionStatus)] =
    engine.Step.status(step) match {
      case StepState.Pending => pendingConfigStatus(step.executions)
      case _                 => configStatus(step.executions)
    }

  protected[server] def observeStatus(executions: List[List[engine.Action]]): ActionStatus =
    executions.flatten.find(_.kind === ActionType.Observe).map(a => actionStateToStatus(a.state.runState)).getOrElse(ActionStatus.Pending)

  def viewStep(step: engine.Step): StandardStep = {
    val configStatus = stepConfigStatus(step)
    StandardStep(
      id = step.id,
      config = step.config,
      status = engine.Step.status(step),
      breakpoint = step.breakpoint.self,
      skip = step.skipMark.self,
      configStatus = configStatus,
      observeStatus = observeStatus(step.executions),
      fileId = step.fileId
    )
  }

  private def decodeTops(s: String): Map[String, String] =
    s.split("=|,").grouped(2).collect {
      case Array(k, v) => k.trim -> v.trim
    }.toMap

  private def initSmartGCal(smartGCalHost: String, smartGCalLocation: String): IO[edu.gemini.seqexec.odb.TrySeq[Unit]] = {
    // SmartGCal always talks to GS
    val peer = new Peer(smartGCalHost, 8443, Site.GS)
    IO.apply(Paths.get(smartGCalLocation)).map { p => SmartGcal.initialize(peer, p) }
  }

  // scalastyle:off
  def seqexecConfiguration: Kleisli[IO, Config, Settings] = Kleisli { cfg: Config =>
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
    val gnirsKeywords            = cfg.require[Boolean]("seqexec-engine.gnirsKeywords")
    val instForceError          = cfg.require[Boolean]("seqexec-engine.instForceError")
    val failAt                  = cfg.require[Int]("seqexec-engine.failAt")
    val odbQueuePollingInterval = Duration(cfg.require[String]("seqexec-engine.odbQueuePollingInterval"))
    val tops                    = decodeTops(cfg.require[String]("seqexec-engine.tops"))
    val caAddrList              = cfg.lookup[String]("seqexec-engine.epics_ca_addr_list")
    val ioTimeout               = Duration(cfg.require[String]("seqexec-engine.ioTimeout"))
    val smartGCalHost           = cfg.require[String]("seqexec-engine.smartGCalHost")
    val smartGCalDir            = cfg.require[String]("seqexec-engine.smartGCalDir")
    val smartGcalEnable         = cfg.lookup[Boolean]("seqexec-engine.smartGCalEnable").getOrElse(true)

    // TODO: Review initialization of EPICS systems
    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def initEpicsSystem[T](sys: EpicsSystem[T], tops: Map[String, String]): IO[Unit] =
      IO.apply(
        Option(CaService.getInstance()) match {
          case None => throw new Exception("Unable to start EPICS service.")
          case Some(s) =>
            sys.init(s, tops).leftMap {
                case SeqexecFailure.SeqexecException(ex) => throw ex
                case c: SeqexecFailure                   => throw new Exception(SeqexecFailure.explain(c))
            }
        }
      ) *> IO.unit

    // Ensure there is a valid way to init CaService either from
    // the configuration file or from the environment
    val caInit   = caAddrList.map(a => IO.apply(CaService.setAddressList(a))).getOrElse {
      IO.apply(Option(System.getenv("EPICS_CA_ADDR_LIST"))).flatMap {
        case Some(_) => IO.unit
        case _       => IO.raiseError(new RuntimeException("Cannot initialize EPICS subsystem"))
      }
    } *> IO.apply(CaService.setIOTimeout(java.time.Duration.ofMillis(ioTimeout.toMillis)))
    val tcsInit  = (tcsKeywords || !tcsSim).fold(initEpicsSystem(TcsEpics, tops), IO.unit)
    // More instruments to be added to the list here
    val instList = site match {
      case Site.GS => List((f2Keywords, Flamingos2Epics), (gmosKeywords, GmosEpics))
      case Site.GN => List((gmosKeywords, GmosEpics), (gnirsKeywords, GnirsEpics))
    }
    val instInit: IO[List[Unit]] = instList.filter(_._1 || !instSim).map(x => initEpicsSystem(x._2, tops)).parSequence
    val gwsInit  = gwsKeywords.fold(initEpicsSystem(GwsEpics, tops), IO.unit)
    val gcalInit = (gcalKeywords || !gcalSim).fold(initEpicsSystem(GcalEpics, tops), IO.unit)
    val smartGcal = smartGcalEnable.fold(initSmartGCal(smartGCalHost, smartGCalDir), IO.unit)

    smartGcal *>
      caInit *>
      tcsInit *>
      gwsInit *>
      gcalInit *>
      instInit *>
      (for {
        now <- IO(LocalDate.now)
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
                       gnirsKeywords,
                       instForceError,
                       failAt,
                       odbQueuePollingInterval)
      )


  }
  // scalastyle:on

}
