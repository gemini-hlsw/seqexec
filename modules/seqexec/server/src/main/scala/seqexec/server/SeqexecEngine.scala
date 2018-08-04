// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import gem.Observation
import gem.enum.Site
import giapi.client.Giapi
import giapi.client.gpi.GPIClient
import seqexec.engine
import seqexec.engine.Result.{FileIdAllocated, Partial}
import seqexec.engine.{Step => _, _}
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.events._
import seqexec.model.{ActionType, UserDetails}
import seqexec.server.ConfigUtilOps._
import seqexec.server.keywords._
import seqexec.server.flamingos2.{Flamingos2ControllerEpics, Flamingos2ControllerSim, Flamingos2ControllerSimBad, Flamingos2Epics}
import seqexec.server.gcal.{GcalControllerEpics, GcalControllerSim, GcalEpics}
import seqexec.server.gmos.{GmosControllerSim, GmosEpics, GmosNorthControllerEpics, GmosSouthControllerEpics}
import seqexec.server.gnirs.{GnirsControllerEpics, GnirsControllerSim, GnirsEpics}
import seqexec.server.gpi.GPIController
import seqexec.server.gws.GwsEpics
import seqexec.server.tcs.{TcsControllerEpics, TcsControllerSim, TcsEpics}
import edu.gemini.seqexec.odb.SmartGcal
import edu.gemini.spModel.core.{Peer, SPProgramID}
import edu.gemini.spModel.obscomp.InstConstants
import edu.gemini.spModel.seqcomp.SeqConfigNames.OCS_KEY
import fs2.{Scheduler, Stream}
import org.http4s.client.Client
import org.http4s.Uri
import knobs.Config
import mouse.all._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class SeqexecEngine(httpClient: Client[IO], settings: SeqexecEngine.Settings, sm: SeqexecMetrics) {
  import SeqexecEngine._

  val odbProxy: ODBProxy = new ODBProxy(new Peer(settings.odbHost, 8443, null),
    if (settings.odbNotifications) ODBProxy.OdbCommandsImpl(new Peer(settings.odbHost, 8442, null))
    else ODBProxy.DummyOdbCommands)

  val gpiGDS: GDSClient = GDSClient(settings.gpiGdsControl.command.fold(httpClient, GDSClient.alwaysOkClient), settings.gpiGDS)

  private val systems = SeqTranslate.Systems(
    odbProxy,
    settings.dhsControl.command.fold(DhsClientHttp(settings.dhsURI), DhsClientSim(settings.date)),
    settings.tcsControl.command.fold(TcsControllerEpics, TcsControllerSim),
    settings.gcalControl.command.fold(GcalControllerEpics, GcalControllerSim),
    settings.f2Control.command.fold(Flamingos2ControllerEpics,
      settings.instForceError.fold(Flamingos2ControllerSimBad(settings.failAt), Flamingos2ControllerSim)),
    settings.gmosControl.command.fold(GmosSouthControllerEpics, GmosControllerSim.south),
    settings.gmosControl.command.fold(GmosNorthControllerEpics, GmosControllerSim.north),
    settings.gnirsControl.command.fold(GnirsControllerEpics, GnirsControllerSim),
    GPIController(new GPIClient(settings.gpiGiapi), gpiGDS)
  )

  private val translatorSettings = SeqTranslate.Settings(
    tcsKeywords = settings.tcsControl.realKeywords,
    f2Keywords = settings.f2Control.realKeywords,
    gwsKeywords = settings.gwsControl.realKeywords,
    gcalKeywords = settings.gcalControl.realKeywords,
    gmosKeywords = settings.gmosControl.realKeywords,
    gnirsKeywords = settings.gnirsControl.realKeywords
  )

  private val translator = SeqTranslate(settings.site, systems, translatorSettings)

  def load(q: EventQueue, seqId: Observation.Id): IO[Either[SeqexecFailure, Unit]] =
    loadEvents(seqId).flatMapF(b => q.enqueue(Stream.emits(b)).map(_.asRight).compile.last.attempt.map(_.bimap(SeqexecFailure.SeqexecException.apply, _ => ()))).value

  def start(q: EventQueue, id: Observation.Id, user: UserDetails, clientId: ClientID): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.start(id, user, clientId)).map(_.asRight)

  def requestPause(q: EventQueue, id: Observation.Id, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.pause(id, user)).map(_.asRight)

  def requestCancelPause(q: EventQueue, id: Observation.Id, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.cancelPause(id, user)).map(_.asRight)

  def setBreakpoint(q: EventQueue,
                    seqId: Observation.Id,
                    user: UserDetails,
                    stepId: seqexec.engine.Step.Id,
                    v: Boolean): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.breakpoint(seqId, user, stepId, v)).map(_.asRight)

  def setOperator(q: EventQueue, user: UserDetails, name: Operator): IO[Either[SeqexecFailure, Unit]] =
     q.enqueue1(Event.logDebugMsg(s"SeqexecEngine: Setting Operator name to '$name' by ${user.username}")) *>
     q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((Engine.State.userData ^|-> EngineMetadata.operator).set(name.some), SetOperator(name, user.some))).map(_.asRight)

  def setObserver(q: EventQueue,
                  seqId: Observation.Id,
                  user: UserDetails,
                  name: Observer): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.setObserver(seqId, user, name)).map(_.asRight)

  def setSelectedSequences(q: EventQueue, i: Instrument, sid: Observation.Id, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Updating loaded sequences")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((Engine.State.userData ^|-> EngineMetadata.selectedML(i)).set(sid.some), SetSelectedSequence(i, sid, user.some))).map(_.asRight)

  def setConditions(q: EventQueue, conditions: Conditions, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting conditions")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((Engine.State.userData ^|-> EngineMetadata.conditions).set(conditions), SetConditions(conditions, user.some))).map(_.asRight)

  def setImageQuality(q: EventQueue, iq: ImageQuality, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting image quality")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((Engine.State.userData ^|-> EngineMetadata.conditions ^|-> Conditions.iq).set(iq), SetImageQuality(iq, user.some))).map(_.asRight)

  def setWaterVapor(q: EventQueue, wv: WaterVapor, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting water vapor")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((Engine.State.userData ^|-> EngineMetadata.conditions ^|-> Conditions.wv).set(wv), SetWaterVapor(wv, user.some))).map(_.asRight)

  def setSkyBackground(q: EventQueue, sb: SkyBackground, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting sky background")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((Engine.State.userData ^|-> EngineMetadata.conditions ^|-> Conditions.sb).set(sb), SetSkyBackground(sb, user.some))).map(_.asRight)

  def setCloudCover(q: EventQueue, cc: CloudCover, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting cloud cover")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((Engine.State.userData ^|-> EngineMetadata.conditions ^|-> Conditions.cc).set(cc), SetCloudCover(cc, user.some))).map(_.asRight)

  def setSkipMark(q: EventQueue,
                  seqId: Observation.Id,
                  user: UserDetails,
                  stepId: seqexec.engine.Step.Id,
                  v: Boolean): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.skip(seqId, user, stepId, v)).map(_.asRight)

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
        toSeqexecEvent(ev, qState)(
          SequencesQueue(
            (Engine.State.userData ^|-> EngineMetadata.selected).get(qState),
            (Engine.State.userData ^|-> EngineMetadata.conditions).get(qState),
            (Engine.State.userData ^|-> EngineMetadata.operator).get(qState),
            qState.sequences.values.map(
              s => viewSequence(s.toSequence, s)
            ).toList
          )
        )
    }
  }

  def stopObserve(q: EventQueue, seqId: Observation.Id): IO[Unit] = q.enqueue1(
    Event.actionStop[executeEngine.ConcreteTypes](seqId, translator.stopObserve(seqId))
  )

  def abortObserve(q: EventQueue, seqId: Observation.Id): IO[Unit] = q.enqueue1(
    Event.actionStop[executeEngine.ConcreteTypes](seqId, translator.abortObserve(seqId))
  )

  def pauseObserve(q: EventQueue, seqId: Observation.Id): IO[Unit] = q.enqueue1(
    Event.actionStop[executeEngine.ConcreteTypes](seqId, translator.pauseObserve)
  )

  def resumeObserve(q: EventQueue, seqId: Observation.Id): IO[Unit] = q.enqueue1(
    Event.getSeqState[executeEngine.ConcreteTypes](seqId, translator.resumePaused(seqId))
  )

  private def addSeq(name: String, seqId: Observation.Id)(st: executeEngine.StateType): executeEngine.StateType = (
    for {
      q   <- st.userData.queues.get(name)
      seq <- st.sequences.get(seqId)
    } yield if(!q.status(st).isRunning && !seq.status.isRunning && !seq.status.isCompleted && !q.contains(seqId)) st.copy(userData = EngineMetadata.queues.modify(_.updated(name, q :+ seqId))(st.userData)) else st
  ).getOrElse(st)

  def addSequenceToQueue(q: EventQueue, queueName: String, seqId: Observation.Id): IO[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[executeEngine.ConcreteTypes](addSeq(queueName, seqId), NullSeqEvent)
  ).map(_.asRight)

  private def removeSeq(name: String, seqId: Observation.Id)(st: executeEngine.StateType): executeEngine.StateType = (
    for {
      q <- st.userData.queues.get(name)
    } yield if(!q.status(st).isRunning && q.contains(seqId)) st.copy(userData = EngineMetadata.queues.modify(_.updated(name, q.filterNot(_===seqId)))(st.userData)) else st
  ).getOrElse(st)

  def removeSequenceFromQueue(q: EventQueue, queueName: String, seqId: Observation.Id): IO[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[executeEngine.ConcreteTypes](removeSeq(queueName, seqId), NullSeqEvent)
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

  private def moveSeq(name: String, seqId: Observation.Id, d: Int)(st: executeEngine.StateType): executeEngine.StateType = (
    for {
      q <- st.userData.queues.get(name)
    } yield if(!q.status(st).isRunning && q.contains(seqId)) st.copy(userData = EngineMetadata.queues.modify(_.updated(name, moveElement(q, seqId, d)))(st.userData)) else st
  ).getOrElse(st)

  def moveSequenceInQueue(q: EventQueue, queueName: String, seqId: Observation.Id, d: Int): IO[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[executeEngine.ConcreteTypes](moveSeq(queueName, seqId, d), NullSeqEvent)
  ).map(_.asRight)

  def notifyODB(i: (executeEngine.EventType, executeEngine.StateType)): IO[(executeEngine.EventType, executeEngine.StateType)] = {
    (i match {
      case (EventSystem(Failed(id, _, e)), _) => systems.odb.obsAbort(id, e.msg)
      case (EventSystem(Executed(id)), st) if st.sequences.get(id).exists(_.status === SequenceState.Idle) =>
        systems.odb.obsPause(id, "Sequence paused by user")
      case (EventSystem(Finished(id)), _)     => systems.odb.sequenceEnd(id)
      case _                                  => SeqAction(())
    }).value.map(_ => i)
  }

  private def loadEvents(seqId: Observation.Id): SeqAction[List[executeEngine.EventType]] = {
    val t: EitherT[IO, SeqexecFailure, (List[SeqexecFailure], Option[Sequence])] = for {
      odbSeq       <- SeqAction.either(odbProxy.read(seqId))
      progIdString <- SeqAction.either(odbSeq.config.extract(OCS_KEY / InstConstants.PROGRAMID_PROP).as[String].leftMap(ConfigUtilOps.explainExtractError))
      _            <- SeqAction.either(Either.catchNonFatal(IO.pure(SPProgramID.toProgramID(progIdString))).leftMap(e => SeqexecFailure.SeqexecException(e): SeqexecFailure))
    } yield translator.sequence(seqId, odbSeq)

    t.map {
      case (err :: _, None)  => List(Event.logDebugMsg(SeqexecFailure.explain(err)))
      case (errs, Some(seq)) => Event.load(seqId, seq) :: errs.map(e => Event.logDebugMsg(SeqexecFailure.explain(e)))
      case _                 => Nil
    }
  }

  private def modifyStateEvent(v: SeqEvent, svs: => SequencesQueue[SequenceView]): SeqexecEvent = v match {
    case NullSeqEvent                 => NullEvent
    case SetOperator(_, _)            => OperatorUpdated(svs)
    case SetObserver(_, _, _)         => ObserverUpdated(svs)
    case SetSelectedSequence(i, s, _) => SelectedSequenceUpdate(i, s)
    case SetConditions(_, _)          => ConditionsUpdated(svs)
    case SetImageQuality(_, _)        => ConditionsUpdated(svs)
    case SetWaterVapor(_, _)          => ConditionsUpdated(svs)
    case SetSkyBackground(_, _)       => ConditionsUpdated(svs)
    case SetCloudCover(_, _)          => ConditionsUpdated(svs)
  }

  def toSeqexecEvent(ev: executeEngine.EventType, st: executeEngine.StateType)(svs: => SequencesQueue[SequenceView]): SeqexecEvent = ev match {
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
      case engine.ModifyStateF(_, evf)   => modifyStateEvent(evf(st), svs)
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

  private def unloadEvent(seqId: Observation.Id): executeEngine.EventType = Event.unload(seqId)

  private def refreshSequenceList(): executeEngine.StateType => IO[Option[Stream[IO, executeEngine.EventType]]] = (st: executeEngine.StateType) => {
    val seqexecList = st.sequences.keys.toSeq

    def loads(odbList: Seq[Observation.Id]): IO[List[executeEngine.EventType]] =
      odbList.diff(seqexecList).toList.map(id => loadEvents(id)).sequence.map(_.flatten).value.map(_.valueOr(r => List(Event.logDebugMsg(SeqexecFailure.explain(r)))))

    def unloads(odbList: Seq[Observation.Id]): Seq[executeEngine.EventType] =
      seqexecList.diff(odbList).map(id => unloadEvent(id))

    val x = odbProxy.queuedSequences.flatMapF(seqs => loads(seqs).map(ee => (ee ++ unloads(seqs)).asRight)).value
    val y = x.map(_.valueOr(r => List(Event.logWarningMsg(SeqexecFailure.explain(r)))))
    for {
      ee <- y
      _  <- sm.queueSize[IO](st.sequences.size)
    } yield ee.nonEmpty option Stream.emits(ee).evalMap(IO.apply(_))
  }

}

// Configuration stuff
object SeqexecEngine extends SeqexecConfiguration {
  sealed trait GPIKeywords

  object GPIKeywords {
    case object GPIKeywordsSimulated extends GPIKeywords
    case object GPIKeywordsGDS extends GPIKeywords

    implicit val eq: Eq[GPIKeywords] = Eq.fromUniversalEquals
  }

  final case class Settings(site: Site,
                            odbHost: String,
                            date: LocalDate,
                            dhsURI: String,
                            dhsControl: ControlStrategy,
                            f2Control: ControlStrategy,
                            gcalControl: ControlStrategy,
                            ghostControl: ControlStrategy,
                            gmosControl: ControlStrategy,
                            gnirsControl: ControlStrategy,
                            gpiControl: ControlStrategy,
                            gpiGdsControl: ControlStrategy,
                            gsaoiControl: ControlStrategy,
                            gwsControl: ControlStrategy,
                            nifsControl: ControlStrategy,
                            niriControl: ControlStrategy,
                            tcsControl: ControlStrategy,
                            odbNotifications: Boolean,
                            instForceError: Boolean,
                            failAt: Int,
                            odbQueuePollingInterval: Duration,
                            gpiGiapi: Giapi[IO],
                            gpiGDS: Uri)
  def apply(httpClient: Client[IO], settings: Settings, c: SeqexecMetrics): SeqexecEngine = new SeqexecEngine(httpClient, settings, c)

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
    val peer = new Peer(smartGCalHost, 8443, edu.gemini.spModel.core.Site.GS)
    IO.apply(Paths.get(smartGCalLocation)).map { p => SmartGcal.initialize(peer, p) }
  }

  // TODO: Initialization is a bit of a mess, with a mix of effectful and effectless code, and values
  // that should go from one to the other. This should be improved.
  def giapiConnection: Kleisli[IO, Config, Giapi[IO]] = Kleisli { cfg: Config =>
    val gpiControl = cfg.require[ControlStrategy]("seqexec-engine.systemControl.gpi")
    val gpiUrl     = cfg.require[String]("seqexec-engine.gpiUrl")
    if (gpiControl.command) {
      Giapi.giapiConnection[IO](gpiUrl, scala.concurrent.ExecutionContext.Implicits.global).connect
    } else {
      Giapi.giapiConnectionIO.connect
    }
  }

  // scalastyle:off
  def seqexecConfiguration(gpiGiapi: Giapi[IO]): Kleisli[IO, Config, Settings] = Kleisli { cfg: Config =>
    val site                    = cfg.require[Site]("seqexec-engine.site")
    val odbHost                 = cfg.require[String]("seqexec-engine.odb")
    val dhsServer               = cfg.require[String]("seqexec-engine.dhsServer")
    val dhsControl              = cfg.require[ControlStrategy]("seqexec-engine.systemControl.dhs")
    val f2Control               = cfg.require[ControlStrategy]("seqexec-engine.systemControl.f2")
    val gcalControl             = cfg.require[ControlStrategy]("seqexec-engine.systemControl.gcal")
    val ghostControl            = cfg.require[ControlStrategy]("seqexec-engine.systemControl.ghost")
    val gmosControl             = cfg.require[ControlStrategy]("seqexec-engine.systemControl.gmos")
    val gnirsControl            = cfg.require[ControlStrategy]("seqexec-engine.systemControl.gnirs")
    val gpiControl              = cfg.require[ControlStrategy]("seqexec-engine.systemControl.gpi")
    val gpiGdsControl           = cfg.require[ControlStrategy]("seqexec-engine.systemControl.gpiGds")
    val gsaoiControl            = cfg.require[ControlStrategy]("seqexec-engine.systemControl.gsaoi")
    val gwsControl              = cfg.require[ControlStrategy]("seqexec-engine.systemControl.gws")
    val nifsControl             = cfg.require[ControlStrategy]("seqexec-engine.systemControl.nifs")
    val niriControl             = cfg.require[ControlStrategy]("seqexec-engine.systemControl.niri")
    val tcsControl              = cfg.require[ControlStrategy]("seqexec-engine.systemControl.tcs")
    val odbNotifications        = cfg.require[Boolean]("seqexec-engine.odbNotifications")
    val gpiGDS                  = cfg.require[Uri]("seqexec-engine.gpiGDS")
    val instForceError          = cfg.require[Boolean]("seqexec-engine.instForceError")
    val failAt                  = cfg.require[Int]("seqexec-engine.failAt")
    val odbQueuePollingInterval = cfg.require[Duration]("seqexec-engine.odbQueuePollingInterval")
    val tops                    = decodeTops(cfg.require[String]("seqexec-engine.tops"))
    val caAddrList              = cfg.lookup[String]("seqexec-engine.epics_ca_addr_list")
    val ioTimeout               = cfg.require[Duration]("seqexec-engine.ioTimeout")
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

    // More instruments to be added to the list here
    val epicsInstruments = site match {
      case Site.GS => List((f2Control, Flamingos2Epics), (gmosControl, GmosEpics))
      case Site.GN => List((gmosControl, GmosEpics), (gnirsControl, GnirsEpics))
    }
    val epicsSystems = epicsInstruments ++ List(
      (tcsControl, TcsEpics),
      (gwsControl, GwsEpics),
      (gcalControl, GcalEpics)
    )
    val epicsInit: IO[List[Unit]] = caInit *> epicsSystems.filter(_._1.connect).map(x => initEpicsSystem(x._2, tops)).parSequence

    val smartGcal = smartGcalEnable.fold(initSmartGCal(smartGCalHost, smartGCalDir), IO.unit)

    smartGcal *>
      epicsInit *>
      (for {
        now <- IO(LocalDate.now)
      } yield Settings(site,
                       odbHost,
                       now,
                       dhsServer,
                       dhsControl,
                       f2Control,
                       gcalControl,
                       ghostControl,
                       gmosControl,
                       gnirsControl,
                       gpiControl,
                       gpiGdsControl,
                       gsaoiControl,
                       gwsControl,
                       nifsControl,
                       niriControl,
                       tcsControl,
                       odbNotifications,
                       instForceError,
                       failAt,
                       odbQueuePollingInterval,
                       gpiGiapi,
                       gpiGDS)
      )


  }
  // scalastyle:on

}
