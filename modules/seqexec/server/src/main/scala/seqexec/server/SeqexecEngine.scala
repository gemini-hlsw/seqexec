// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import java.nio.file.Paths
import java.time.LocalDate
import java.util.concurrent.TimeUnit

import cats._
import cats.data.{Kleisli, StateT}
import cats.effect.{IO, Sync}
import cats.implicits._
import monocle.Monocle._
import monocle.Optional
import edu.gemini.epics.acm.CaService
import gem.Observation
import gem.enum.Site
import giapi.client.Giapi
import giapi.client.gpi.GPIClient
import seqexec.engine
import seqexec.engine.Result.Partial
import seqexec.engine.{Step => _, _}
import seqexec.engine.Handle
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.events._
import seqexec.model.{ActionType, UserDetails, StepId}
import seqexec.server.ConfigUtilOps._
import seqexec.server.keywords._
import seqexec.server.flamingos2.{Flamingos2ControllerEpics, Flamingos2ControllerSim, Flamingos2ControllerSimBad, Flamingos2Epics}
import seqexec.server.gcal.{GcalControllerEpics, GcalControllerSim, GcalEpics}
import seqexec.server.ghost.GHOSTController
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
import giapi.client.ghost.GHOSTClient
import org.http4s.client.Client
import org.http4s.Uri
import knobs.Config
import mouse.all._
import seqexec.model.dhs.ImageFileId

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import shapeless.tag.@@
import shapeless.tag

class SeqexecEngine(httpClient: Client[IO], settings: Settings[IO], sm: SeqexecMetrics) {
  import SeqexecEngine._

  val odbProxy: ODBProxy = new ODBProxy(new Peer(settings.odbHost, 8443, null),
    if (settings.odbNotifications) ODBProxy.OdbCommandsImpl(new Peer(settings.odbHost, 8442, null))
    else ODBProxy.DummyOdbCommands)

  val gpiGDS: GDSClient = GDSClient(settings.gpiGdsControl.command.fold(httpClient, GDSClient.alwaysOkClient), settings.gpiGDS)

  val ghostGDS: GDSClient = GDSClient(settings.ghostControl.command.fold(httpClient, GDSClient.alwaysOkClient), settings.ghostGDS)

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
    GPIController(new GPIClient(settings.gpiGiapi), gpiGDS),
    GHOSTController(new GHOSTClient(settings.ghostGiapi), ghostGDS)
  )

  private val translatorSettings = TranslateSettings(
    tcsKeywords = settings.tcsControl.realKeywords,
    f2Keywords = settings.f2Control.realKeywords,
    gwsKeywords = settings.gwsControl.realKeywords,
    gcalKeywords = settings.gcalControl.realKeywords,
    gmosKeywords = settings.gmosControl.realKeywords,
    gnirsKeywords = settings.gnirsControl.realKeywords
  )

  private val translator = SeqTranslate(settings.site, systems, translatorSettings)

  def load(q: EventQueue, seqId: Observation.Id): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue(Stream.emits(loadEvents(seqId))).map(_.asRight).compile.last.attempt.map(_.bimap(SeqexecFailure.SeqexecException.apply, _ => ()))

  // TODO: this is too much guessing. We should have proper tracking of systems' state.
  def failedInstruments(st: EngineState): Set[Resource] = st.sequences.values.toList.mapFilter(s =>
    s.seq.status.isError.option(s.seqGen.instrument)).toSet

  private def checkResources(seqId: Observation.Id)(st: EngineState): Boolean = {
    // Resources used by running sequences
    val used = resourcesInUse(st)

    // Resources that will be used by sequences in running queues
    val reservedByQueues = resourcesReserved(failedInstruments(st), st)

    st.sequences.get(seqId).exists(_.seqGen.resources.intersect(used ++ reservedByQueues).isEmpty)

  }

  def start(q: EventQueue, id: Observation.Id, user: UserDetails, clientId: ClientId): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.start[executeEngine.ConcreteTypes](id, user, clientId, checkResources(id))).map(_.asRight)

  def requestPause(q: EventQueue, id: Observation.Id, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.pause(id, user)).map(_.asRight)

  def requestCancelPause(q: EventQueue, id: Observation.Id, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.cancelPause(id, user)).map(_.asRight)

  def setBreakpoint(q: EventQueue,
                    seqId: Observation.Id,
                    user: UserDetails,
                    stepId: StepId,
                    v: Boolean): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.breakpoint(seqId, user, stepId, v)).map(_.asRight)

  def setOperator(q: EventQueue, user: UserDetails, name: Operator): IO[Either[SeqexecFailure,
    Unit]] = q.enqueue1(Event.logDebugMsg(s"SeqexecEngine: Setting Operator name to '$name' by " +
    s"${user.username}")) *> q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes](
    (EngineState.operator.set(name.some) >>> refreshSequences withEvent SetOperator(name,
      user.some)).toHandle)).map(_.asRight)

  def setObserver(q: EventQueue,
                  seqId: Observation.Id,
                  user: UserDetails,
                  name: Observer): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg(s"SeqexecEngine: Setting Observer name to '$name' for sequence '${seqId.format}' by ${user.username}")) *>
        q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes](
          ((EngineState.sequences ^|-? index(seqId)).modify(SequenceData.observer.set(name.some)) >>> refreshSequence(seqId) withEvent SetObserver(seqId, user.some, name)).toHandle)).map(_.asRight)

  def selectSequenceEvent(i: Instrument, sid: Observation.Id, observer: Observer, user: UserDetails, clientId: ClientId): executeEngine.EventType= {
    val lens =
      (EngineState.sequences ^|-? index(sid)).modify(SequenceData.observer.set(observer.some)) >>>
       EngineState.instrumentLoadedL(i).set(sid.some) >>>
       refreshSequence(sid)
    def testRunning(st: EngineState):Boolean = (for {
      sels   <- st.selected.get(i)
      obsseq <- st.sequences.get(sels)
    } yield obsseq.seq.status.isRunning).getOrElse(false)

    Event.modifyState[executeEngine.ConcreteTypes]{ ((st: EngineState) => {
      if (!testRunning(st)) (lens withEvent AddLoadedSequence(i, sid, user, clientId))(st)
      else (st, NotifyUser(InstrumentInUse(sid, i), clientId))
    }).toHandle }
  }

  def selectSequence(q: EventQueue, i: Instrument, sid: Observation.Id, observer: Observer, user: UserDetails, clientId: ClientId): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logInfoMsg(s"User '${user.displayName}' loads sequence ${sid.format} on ${i.show}")) *>
    q.enqueue1(selectSequenceEvent(i, sid, observer, user, clientId)).map(_.asRight)

  def clearLoadedSequences(q: EventQueue, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Updating loaded sequences")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((EngineState.selected.set(Map.empty) withEvent ClearLoadedSequences(user.some)).toHandle)).map(_.asRight)

  def setConditions(q: EventQueue, conditions: Conditions, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting conditions")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes]((EngineState.conditions.set(conditions) >>> refreshSequences withEvent SetConditions(conditions, user.some)).toHandle)).map(_.asRight)

  def setImageQuality(q: EventQueue, iq: ImageQuality, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting image quality")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes](((EngineState.conditions ^|-> Conditions.iq).set(iq) >>> refreshSequences withEvent SetImageQuality(iq, user.some)).toHandle)).map(_.asRight)

  def setWaterVapor(q: EventQueue, wv: WaterVapor, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting water vapor")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes](((EngineState.conditions ^|-> Conditions.wv).set(wv) >>> refreshSequences withEvent SetWaterVapor(wv, user.some)).toHandle)).map(_.asRight)

  def setSkyBackground(q: EventQueue, sb: SkyBackground, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting sky background")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes](((EngineState.conditions ^|-> Conditions.sb).set(sb) >>> refreshSequences withEvent SetSkyBackground(sb, user.some)).toHandle)).map(_.asRight)

  def setCloudCover(q: EventQueue, cc: CloudCover, user: UserDetails): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting cloud cover")) *>
    q.enqueue1(Event.modifyState[executeEngine.ConcreteTypes](((EngineState.conditions ^|-> Conditions.cc).set(cc) >>> refreshSequences withEvent SetCloudCover(cc, user.some)).toHandle)).map(_.asRight)

  def setSkipMark(q: EventQueue,
                  seqId: Observation.Id,
                  user: UserDetails,
                  stepId: StepId,
                  v: Boolean): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.skip(seqId, user, stepId, v)).map(_.asRight)

  def requestRefresh(q: EventQueue, clientId: ClientId): IO[Unit] = q.enqueue1(Event.poll(clientId))

  def seqQueueRefreshStream: Stream[IO, executeEngine.EventType] =
    Scheduler[IO](corePoolSize = 1).flatMap { scheduler =>
      val fd = Duration(settings.odbQueuePollingInterval.toSeconds, TimeUnit.SECONDS)
      scheduler.fixedDelay[IO](fd).evalMap(_ => odbProxy.queuedSequences.value).map { x =>
        Event.getState[executeEngine.ConcreteTypes](st =>
          x.map(refreshSequenceList(_)(st)).valueOr(r =>
            List(Event.logWarningMsg(SeqexecFailure.explain(r)))
          ).some.filter(_.nonEmpty).map(Stream.emits(_).covary[IO])
        )
      }
    }

  private def executionQueueViews(st: EngineState): SortedMap[QueueId, ExecutionQueueView] = {
    SortedMap(st.queues.map {
      case (qid, q) => qid -> ExecutionQueueView(qid, q.name, q.cmdState, q.status(st), q.queue)
    }.toList: _*)
  }

  def eventStream(q: EventQueue): Stream[IO, SeqexecEvent] = {
    stream(q.dequeue.mergeHaltBoth(seqQueueRefreshStream))(EngineState.default).flatMap(x =>
      Stream.eval(notifyODB(x))).flatMap {
        case (ev, qState) =>
          val sequences = qState.sequences.values.map(viewSequence).toList
          val event = toSeqexecEvent(ev)(
            SequencesQueue(
              EngineState.selected.get(qState),
              EngineState.conditions.get(qState),
              EngineState.operator.get(qState),
              executionQueueViews(qState),
              sequences
            )
          )
          Stream.eval(updateMetrics[IO](ev, sequences).map(_ => event))
    }
  }

  private[server] def stream(p: Stream[IO, executeEngine.EventType])(s0: EngineState)
  : Stream[IO, (executeEngine.ResultType, EngineState)] =
    executeEngine.process(iterateQueues)(p)(s0)

  def stopObserve(q: EventQueue, seqId: Observation.Id): IO[Unit] = q.enqueue1(
    Event.actionStop[executeEngine.ConcreteTypes](seqId, translator.stopObserve(seqId))
  )

  def abortObserve(q: EventQueue, seqId: Observation.Id): IO[Unit] = q.enqueue1(
    Event.actionStop[executeEngine.ConcreteTypes](seqId, translator.abortObserve(seqId))
  )

  def pauseObserve(q: EventQueue, seqId: Observation.Id): IO[Unit] = q.enqueue1(
    Event.actionStop[executeEngine.ConcreteTypes](seqId, translator.pauseObserve(seqId))
  )

  def resumeObserve(q: EventQueue, seqId: Observation.Id): IO[Unit] = q.enqueue1(
    Event.getState[executeEngine.ConcreteTypes](translator.resumePaused(seqId))
  )

  def queueO(qid: QueueId): Optional[EngineState, ExecutionQueue] =
    EngineState.queues ^|-? index(qid)

  def cmdStateO(qid: QueueId): Optional[EngineState, BatchCommandState] =
    queueO(qid) ^|-> ExecutionQueue.cmdState

  private def addSeqs(qid: QueueId, seqIds: List[Observation.Id]): executeEngine.HandleType[Unit] =
    executeEngine.get.flatMap{ st => (
      for {
        q <- st.queues.get(qid)
        seqs <- seqIds.filter(sid => st.sequences.get(sid)
          .exists(os => !os.seq.status.isRunning && !os.seq.status.isCompleted && !q.queue.contains
          (sid))
        ).some.filter(_.nonEmpty)
        if seqs.nonEmpty
      } yield executeEngine.modify(queueO(qid).modify(_.addSeqs(seqs))) *>
        ((q.cmdState, q.status(st)) match {
          case (_, BatchExecState.Completed)       => ((EngineState.queues ^|-? index(qid) ^|-> ExecutionQueue.cmdState)
            .set(BatchCommandState.Idle) >>> {(_, ())}).toHandle
          case (BatchCommandState.Run(o, u, c), _) => runQueue(qid, o, u, c)
          case _                                   => executeEngine.unit
        })
    ).getOrElse(executeEngine.unit)}

  def addSequencesToQueue(q: EventQueue, qid: QueueId, seqIds: List[Observation.Id])
  : IO[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[executeEngine.ConcreteTypes](addSeqs(qid, seqIds)
      .map[executeEngine.ConcreteTypes#EventData](_ => UpdateQueueAdd(qid, seqIds)))
  ).map(_.asRight)

  def addSequenceToQueue(q: EventQueue, qid: QueueId, seqId: Observation.Id): IO[Either[SeqexecFailure, Unit]] =
    addSequencesToQueue(q, qid, List(seqId))

  private def removeSeq(qid: QueueId, seqId: Observation.Id): executeEngine.HandleType[Unit] =
    executeEngine.get.flatMap{ st => (
      for {
        q <- st.queues.get(qid)
        if q.queue.contains(seqId)
        sstOp = st.sequences.get(seqId).map(_.seq.status)
        if q.status(st) =!= BatchExecState.Running ||
          sstOp.forall(sst => !sst.isRunning && !sst.isCompleted)
      } yield executeEngine.modify(queueO(qid).modify(_.removeSeq(seqId))) *>
        ((q.cmdState, q.status(st)) match {
          case (_, BatchExecState.Completed)         => executeEngine.unit
          case (BatchCommandState.Run(o, u, c), _)
            if sstOp.exists(_.isError) => runQueue(qid, o, u, c)
          case _                                     => executeEngine.unit
        })
    ).getOrElse(executeEngine.unit)}

  def removeSequenceFromQueue(q: EventQueue, qid: QueueId, seqId: Observation.Id)
  : IO[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[executeEngine.ConcreteTypes](
      executeEngine.get.flatMap(st => removeSeq(qid, seqId)
        .map(_ => UpdateQueueRemove(qid, List(seqId), st.queues.get(qid)
          .map(_.queue.indexOf(seqId)).toList))))
  ).map(_.asRight)

  private def moveSeq(qid: QueueId, seqId: Observation.Id, delta: Int): Endo[EngineState] = st => (
    st.queues.get(qid).filter(_.queue.contains(seqId)).map {_ =>
      queueO(qid).modify(_.moveSeq(seqId, delta))(st)
    }
  ).getOrElse(st)

  def moveSequenceInQueue(q: EventQueue, qid: QueueId, seqId: Observation.Id, delta: Int, cid: ClientId)
  : IO[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[executeEngine.ConcreteTypes](
      executeEngine.get.flatMap(_ => (moveSeq(qid, seqId, delta) withEvent UpdateQueueMoved(qid,
        cid, seqId, 0)).toHandle))
    ).map(_.asRight)

  private def clearQ(qid: QueueId): Endo[EngineState] = st => (
    st.queues.get(qid).filter(_.status(st) =!= BatchExecState.Running).map { _ =>
      queueO(qid).modify(_.clear)(st)
    }
  ).getOrElse(st)

  def clearQueue(q: EventQueue, qid: QueueId): IO[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[executeEngine.ConcreteTypes](
      (clearQ(qid) withEvent UpdateQueueClear(qid)).toHandle)
  ).map(_.asRight)


  /* Most of the magic for the ExecutionQueue is done here and in nextRunnableObservations.
   * runQueue finds the next eligible sequences in queue qid, and starts them. If called in a queue
   * that already have all possible sequences running, it does nothing.
   */
  private def runQueue(qid: QueueId, observer: Observer, user: UserDetails, clientId: ClientId): executeEngine.HandleType[Unit] = {
    def setObserverAndSelect(sid: Observation.Id): executeEngine.HandleType[Unit] = Handle(StateT[IO, EngineState, (Unit, Option[Stream[IO, executeEngine.EventType]])]{ st:EngineState => IO(
      (EngineState.sequences ^|-? index(sid)).getOption(st).map{ obsseq =>
        (EngineState.sequences.modify(_ + (sid -> obsseq.copy(observer = observer.some))) >>>
          refreshSequence(sid) >>>
          EngineState.instrumentLoadedL(obsseq.seqGen.instrument).set(sid.some) >>>
          {(_, ((), Stream[executeEngine.EventType](
            Event.modifyState[executeEngine.ConcreteTypes](
              { {s:EngineState => s} withEvent
                AddLoadedSequence(obsseq.seqGen.instrument, sid, user, clientId)
              }.toHandle
            )
          ).covary[IO].some))}
        )(st)
      }.getOrElse((st, ((), None)))
    )})

    executeEngine.get.map(nextRunnableObservations(qid)).flatMap(_.map(sid =>
      setObserverAndSelect(sid) *> executeEngine.start(sid, clientId, { _ => true }))
      .fold(executeEngine.unit)(_ *> _))
  }

  def startQueue(q: EventQueue, qid: QueueId, observer: Observer, user: UserDetails, clientId: ClientId): IO[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[executeEngine.ConcreteTypes](executeEngine.get.flatMap{ st => {
      queueO(qid).getOption(st).filterNot(_.queue.isEmpty).map {
        _.status(st) match {
          case BatchExecState.Idle |
               BatchExecState.Stopping => ((EngineState.queues ^|-? index(qid) ^|-> ExecutionQueue.cmdState)
            .set(BatchCommandState.Run(observer, user, clientId)) >>> {(_, ())}).toHandle *>
            runQueue(qid, observer, user, clientId)
          case _                       => executeEngine.unit
        }
      }.getOrElse(executeEngine.unit)
    }}.map(_ => StartQueue(qid, clientId)))
  ).map(_.asRight)

  private def stopSequencesInQueue(qid: QueueId): executeEngine.HandleType[Unit] =
    executeEngine.get.map(st =>
      queueO(qid).getOption(st)
        .foldMap(_.queue.filter(sid => EngineState.sequenceStateIndex(sid)
          .getOption(st).exists(_.status.isRunning)))
    ).flatMap(_.map(executeEngine.pause).fold(executeEngine.unit)(_ *> _))

  def stopQueue(q: EventQueue, qid: QueueId, clientId: ClientId): IO[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[executeEngine.ConcreteTypes](executeEngine.get.flatMap{ st =>
      queueO(qid).getOption(st).map {
        _.status(st) match {
          case BatchExecState.Running => (cmdStateO(qid).set(BatchCommandState.Stop) >>> {(_, ())}).toHandle *>
            stopSequencesInQueue(qid)
          case BatchExecState.Waiting => (cmdStateO(qid).set(BatchCommandState.Stop) >>> {(_, ())}).toHandle
          case _                      => executeEngine.unit
        }
      }.getOrElse(executeEngine.unit)
    }.map(_ => StopQueue(qid, clientId)))
  ).map(_.asRight)

  // It assumes only one queue can run at a time
  private val iterateQueues: PartialFunction[SystemEvent, executeEngine.HandleType[Unit]] = {
    case Finished(_) => executeEngine.get.map(st => st.queues.collectFirst {
      case (qid, q@ExecutionQueue(_, BatchCommandState.Run(observer, user, clid), _))
        if q.status(st) =!= BatchExecState.Completed =>
          (qid, observer, user, clid)
    }).flatMap(_.map(Function.tupled(runQueue)).getOrElse(executeEngine.unit))
  }

  private def configSystemCheck(sid: Observation.Id, sys: Resource)(st: EngineState): Boolean = {
    // Resources used by running sequences
    val used = resourcesInUse(st)

    // Resources reserved by running queues, excluding `sid` to prevent self blocking
    val reservedByQueues = resourcesReserved(failedInstruments(st),
      EngineState.sequences.modify(_ - sid)(st))

    !(used ++ reservedByQueues).contains(sys)
  }

  private def configSystemHandle(sid: Observation.Id, stepId: StepId, sys: Resource)
  : executeEngine.HandleType[SeqEvent] = {

    executeEngine.get.flatMap{ st =>
      if(configSystemCheck(sid, sys)(st))
        st.sequences.get(sid).flatMap(_.seqGen.configActionCoord(stepId, sys))
          .map(c => executeEngine.startSingle(ActionCoords(sid, c)).map[SeqEvent]{
            case EventResult.Ok => StartSysConfig(sid, stepId, sys)
            case _              => NullSeqEvent
          }).getOrElse(executeEngine.pure(NullSeqEvent))
      else executeEngine.pure(NullSeqEvent)
    }
  }

  /*
   *  Triggers the application of a specific step configuration to a system
   */
  def configSystem(q: EventQueue, sid: Observation.Id, stepId: StepId, sys: Resource)
  : IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(
      Event.modifyState[executeEngine.ConcreteTypes](configSystemHandle(sid, stepId, sys))
    ).map(_.asRight)



  def notifyODB(i: (executeEngine.ResultType, EngineState)): IO[(executeEngine.ResultType, EngineState)] = {
    (i match {
      case (SystemUpdate(Failed(id, _, e), _), _) => systems.odb.obsAbort(id, e.msg)
      case (SystemUpdate(Executed(id), _), st) if EngineState.sequenceStateIndex(id).getOption(st)
        .exists(_.status === SequenceState.Idle) =>
        systems.odb.obsPause(id, "Sequence paused by user")
      case (SystemUpdate(Finished(id), _), _)     => systems.odb.sequenceEnd(id)
      case _                                  => SeqAction(())
    }).value.map(_ => i)
  }

  private def loadEvents(seqId: Observation.Id): List[executeEngine.EventType] = {
    val t: Either[SeqexecFailure, (List[SeqexecFailure], Option[SequenceGen])] = for {
      odbSeq       <- odbProxy.read(seqId)
      progIdString <- odbSeq.config.extractAs[String](OCS_KEY / InstConstants.PROGRAMID_PROP).leftMap(ConfigUtilOps.explainExtractError)
      _            <- Either.catchNonFatal(IO.pure(SPProgramID.toProgramID(progIdString))).leftMap(e => SeqexecFailure.SeqexecException(e): SeqexecFailure)
    } yield translator.sequence(seqId, odbSeq)

    def loadSequenceEvent(seqg: SequenceGen): executeEngine.EventType =
      Event.modifyState[executeEngine.ConcreteTypes](( { st:EngineState =>
        st.sequences.get(seqId).fold(loadSequenceEndo(seqId, seqg))(
          _ => reloadSequenceEndo(seqId, seqg)
        )(st)
      } withEvent LoadSequence(seqId)).toHandle)

    t.map {
      case (err :: _, None)  => List(Event.logDebugMsg(SeqexecFailure.explain(err)))
      case (errs, Some(seq)) => loadSequenceEvent(seq) :: errs.map(e => Event.logDebugMsg(SeqexecFailure.explain(e)))
      case _                 => Nil
    }.valueOr(e => List(Event.logDebugMsg(SeqexecFailure.explain(e))))
  }

  /**
   * Update some metrics based on the event types
   */
  def updateMetrics[F[_]: Sync](e: executeEngine.ResultType, sequences: List[SequenceView]): F[Unit] = {
    def instrument(id: Observation.Id): Option[Instrument] = sequences.find(_.id === id).map(_.metadata.instrument)
    (e match {
      // TODO Add metrics for more events
      case engine.UserCommandResponse(ue, _, _)   => ue match {
        case engine.Start(id, _, _, _) => instrument(id).map(sm.startRunning[F]).getOrElse(Sync[F].unit)
        case _                         => Sync[F].unit
      }
      case engine.SystemUpdate(se, _) => se match {
        case _ => Sync[F].unit
      }
      case _                      => Sync[F].unit
    }).flatMap(_ => Sync[F].unit)
  }

  def viewSequence(obsSeq: SequenceData): SequenceView = {
    val st = obsSeq.seq
    val seq = st.toSequence

    def resources(s: SequenceGen.StepGen): List[Resource] = s match {
      case SequenceGen.PendingStepGen(_, _, resources, _) => resources.toList
      case _                                              => List.empty
    }
    def engineSteps(seq: Sequence[IO]): List[Step] = {

      obsSeq.seqGen.steps.zip(seq.steps).map{
        case (a, b) => viewStep(a, b, resources(a).mapFilter(x =>
          obsSeq.seqGen.configActionCoord(a.id, x)
            .map(i => (x, actionStateToStatus(obsSeq.seq.getSingleState(i))))
        ))
      }
      match {
        // The sequence could be empty
        case Nil => Nil
        // Find first Pending Step when no Step is Running and mark it as Running
        case steps if Sequence.State.isRunning(st) && steps.forall(_.status =!= StepState.Running) =>
          val (xs, y :: ys) = splitWhere(steps)(_.status === StepState.Pending)
          xs ++ (y.copy(status = StepState.Running) :: ys)
        case steps if st.status === SequenceState.Idle && steps.exists(_.status === StepState.Running) =>
          val (xs, y :: ys) = splitWhere(steps)(_.status === StepState.Running)
          xs ++ (y.copy(status = StepState.Paused) :: ys)
        case x => x
      }
    }

    // TODO: Implement willStopIn
    SequenceView(seq.id, SequenceMetadata(obsSeq.seqGen.instrument, obsSeq.observer, obsSeq.seqGen.title), st.status, engineSteps(seq), None)
  }

  private def unloadEvent(seqId: Observation.Id): executeEngine.EventType =
    Event.modifyState[executeEngine.ConcreteTypes](
      ( { st:EngineState =>
          if (executeEngine.canUnload(seqId)(st)) {
            (EngineState.sequences.modify(ss => ss - seqId) >>>
             EngineState.selected.modify(ss => ss.toList.filter{case (_, x) => x =!= seqId}.toMap) >>>
             EngineState.queues.modify(_.mapValues(ExecutionQueue.queue.modify(_.filterNot(_ === seqId)))))(st)
          }
          else st
      } withEvent UnloadSequence(seqId)).toHandle
    )

  private def refreshSequenceList(odbList: Seq[Observation.Id])(st: EngineState): List[executeEngine.EventType] = {
    val seqexecList = st.sequences.keys.toSeq

    val loads = odbList.diff(seqexecList).flatMap(id => loadEvents(id))

    val unloads = seqexecList.diff(odbList).map(id => unloadEvent(id))

    (loads ++ unloads).toList
  }

  implicit private final class ToHandle[A](f: EngineState => (EngineState, A)) {
    import Handle.StateToHandle
    def toHandle: Handle[EngineState, Event[executeEngine.ConcreteTypes], A] =
      StateT[IO, EngineState, A]{ st => IO(f(st)) }.toHandle
  }

}

object SeqexecEngine extends SeqexecConfiguration {
  def apply(httpClient: Client[IO], settings: Settings[IO], c: SeqexecMetrics): SeqexecEngine = new SeqexecEngine(httpClient, settings, c)

  def splitWhere[A](l: List[A])(p: A => Boolean): (List[A], List[A]) =
    l.splitAt(l.indexWhere(p))

  def splitAfter[A](l: List[A])(p: A => Boolean): (List[A], List[A]) =
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

  private[server] def separateActions(ls: List[Action[IO]]): (List[Action[IO]], List[Action[IO]]) =  ls.partition{ _.state.runState match {
    case engine.Action.Completed(_) => false
    case engine.Action.Failed(_)    => false
    case _                          => true
  } }

  private[server] def configStatus(executions: List[List[engine.Action[IO]]]): List[(Resource, ActionStatus)] = {
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
  private[server] def pendingConfigStatus(executions: List[List[engine.Action[IO]]]): List[(Resource, ActionStatus)] =
    executions.map {
      s => separateActions(s).bimap(_.map(_.kind).flatMap(kindToResource), _.map(_.kind).flatMap(kindToResource))
    }.flatMap {
      x => x._1 ::: x._2
    }.distinct.tupleRight(ActionStatus.Pending).sortBy(_._1)

  /**
   * Overall pending status for a step
   */
  private def stepConfigStatus(step: engine.Step[IO]): List[(Resource, ActionStatus)] =
    engine.Step.status(step) match {
      case StepState.Pending => pendingConfigStatus(step.executions)
      case _                 => configStatus(step.executions)
    }

  private def observeAction(executions: List[List[engine.Action[IO]]]): Option[Action[IO]] =
    executions.flatten.find(_.kind === ActionType.Observe)

  private[server] def observeStatus(executions: List[List[engine.Action[IO]]]): ActionStatus =
    observeAction(executions).map(a => actionStateToStatus(a.state.runState)).getOrElse(
      ActionStatus.Pending)

  private def fileId(executions: List[List[engine.Action[IO]]]): Option[ImageFileId] =
    observeAction(executions).flatMap(_.state.partials.collectFirst{
      case FileIdAllocated(fid) => fid
    })

  def viewStep(stepg: SequenceGen.StepGen, step: engine.Step[IO],
               altCfgStatus: List[(Resource, ActionStatus)]): StandardStep = {
    val status = engine.Step.status(step)
    val configStatus = if(status === StepState.Completed || status === StepState.Running)
      stepConfigStatus(step)
    else
      altCfgStatus

    StandardStep(
      id = step.id,
      config = stepg.config,
      status = status,
      breakpoint = step.breakpoint.self,
      skip = step.skipMark.self,
      configStatus = configStatus,
      observeStatus = observeStatus(step.executions),
      fileId = fileId(step.executions).orElse(stepg.some.collect{
        case SequenceGen.CompletedStepGen(_, _, fileId) => fileId
      }.flatten)
    )
  }

  private def systemsBeingConfigured(st: EngineState): Set[Resource] =
    st.sequences.values.filter(d => d.seq.status.isError || d.seq.status.isIdle).toList
      .flatMap(s => s.seq.getSingleActionStates.filter(_._2.started).keys.toList
        .mapFilter(s.seqGen.resourceAtCoords)
      ).toSet

  /*
   * Resource in use = Resources used by running sequences, plus the systems that are being
   * configured because a user commanded a manual configuration apply.
   */
  private def resourcesInUse(st: EngineState): Set[Resource] =
    st.sequences.values.toList.mapFilter(s => s.seq.status.isRunning.option(s.seqGen.resources))
      .foldK ++
      systemsBeingConfigured(st)

  /*
   * Resources reserved by running queues.
   */
  private def resourcesReserved(failedSystems: Set[Resource], st: EngineState): Set[Resource] = {
    def reserved(q: ExecutionQueue): Set[Resource] = q.queue.fproduct(st.sequences.get).collect{
      case (_, Some(s)) if s.seq.status.isIdle &&
        failedSystems.intersect(s.seqGen.resources).isEmpty => s.seqGen.resources
    }.foldK

    val runningQs = st.queues.values.filter(_.status(st).running)

    runningQs.map(reserved).toList.foldK

  }

  /**
    * Find the observations in an execution queue that would be run next, taking into account the
    * resources required by each observation and the resources currently in use.
    * The order in the queue defines the priority of the observations.
    * Failed sequences in the queue keep their instruments taken, preventing that the queue starts
    * other sequences for those instruments.
    * @param qid The execution queue id
    * @param st The current engine state
    * @return The set of all observations in the execution queue `qid` that can be started to run
    *         in parallel.
    */
  def nextRunnableObservations(qid: QueueId)(st: EngineState): Set[Observation.Id] = {
    // Set of all resources in use
    val used = resourcesInUse(st)
    // For each observation in the queue that is not yet run, retrieve the required resources
    val obs = st.queues.get(qid).map(_.queue.fproduct(st.sequences.get).collect {
      case (id, Some(s)) if !s.seq.status.isRunning && !s.seq.status.isCompleted =>
        id -> s.seqGen.resources
    }).orEmpty

    // Calculate instruments reserved by failed sequences in the queue
    val resFailed: Set[Resource] = st.queues.get(qid).map(_.queue.mapFilter(st.sequences.get(_)
      .flatMap(s => s.seq.status.isError.option(s.seqGen.instrument)))).orEmpty.toSet

    obs.foldLeft((used ++ resFailed, Set[Observation.Id]())){ (b, o) =>
      (o, b) match { case ((oid, res), (u, a)) => if(u.intersect(res).isEmpty) (u ++ res, a + oid) else (u, a) }
    }._2
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
  def giapiConnection[T](controlName: String, urlName: String): Kleisli[IO, Config, Giapi[IO] @@ T] = Kleisli { cfg: Config =>
    val control = cfg.require[ControlStrategy](controlName)
    val url  = cfg.require[String](urlName)
    (if (control.command) {
      Giapi.giapiConnection[IO](url, scala.concurrent.ExecutionContext.Implicits.global).connect
    } else {
      Giapi.giapiConnectionIO(scala.concurrent.ExecutionContext.Implicits.global).connect
    }).map(tag[T][Giapi[IO]](_)) // Tag the connection
  }

  // scalastyle:off
  def seqexecConfiguration(gpiGiapi: Giapi[IO] @@ GpiSettings, ghostGiapi: Giapi[IO] @@ GhostSettings): Kleisli[IO, Config, Settings[IO]] = Kleisli { cfg: Config =>
    val site                    = cfg.require[Site]("seqexec-engine.site")
    val odbHost                 = cfg.require[String]("seqexec-engine.odb")
    val dhsServer               = cfg.require[String]("seqexec-engine.dhsServer")
    val dhsControl              = cfg.require[ControlStrategy]("seqexec-engine.systemControl.dhs")
    val f2Control               = cfg.require[ControlStrategy]("seqexec-engine.systemControl.f2")
    val gcalControl             = cfg.require[ControlStrategy]("seqexec-engine.systemControl.gcal")
    val ghostControl            = cfg.require[ControlStrategy]("seqexec-engine.systemControl.ghost")
    val ghostGdsControl         = cfg.require[ControlStrategy]("seqexec-engine.systemControl.ghostGds")
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
    val gpiGDS                  = tag[GpiSettings][Uri](cfg.require[Uri]("seqexec-engine.gpiGDS"))
    val ghostGDS                = tag[GhostSettings][Uri](cfg.require[Uri]("seqexec-engine.ghostGDS"))
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
                       ghostGdsControl,
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
                       ghostGiapi,
                       gpiGDS,
                       ghostGDS)
      )


  }

  private def toStepList(seq: SequenceGen, d: HeaderExtraData): List[engine.Step[IO]] =
    seq.steps.map(_.generate(d))

  private def toEngineSequence(id: Observation.Id, seq: SequenceGen, d: HeaderExtraData)
  : Sequence[IO] = Sequence(id, toStepList(seq, d))

  private[server] def loadSequenceEndo(seqId: Observation.Id, seqg: SequenceGen)
  : Endo[EngineState] = st =>
    EngineState.sequences.modify(ss => ss + (seqId -> SequenceData(None, seqg,
      executeEngine.load(toEngineSequence(seqId, seqg,
        HeaderExtraData(st.conditions, st.operator, None)
      ))
    )))(st)

  private[server] def reloadSequenceEndo(seqId: Observation.Id, seqg: SequenceGen)
  : Endo[EngineState] = st => EngineState.atSequence(seqId).modify(sd => sd.copy(
    seqGen = seqg,
    seq = executeEngine.reload(sd.seq, toStepList(seqg, HeaderExtraData(st.conditions,
      st.operator, sd.observer
    )))
  ))(st)

  private[server] def updateSequenceEndo(seqId: Observation.Id, obsseq: SequenceData)
  : Endo[EngineState] = st =>
    executeEngine.update(seqId, toStepList(obsseq.seqGen, HeaderExtraData(st.conditions,
      st.operator, obsseq.observer)))(st)

  private def refreshSequence(id: Observation.Id): Endo[EngineState] = (st:EngineState) => {
    st.sequences.get(id).map(obsseq => updateSequenceEndo(id, obsseq)).foldLeft(st){case (s, f) => f(s)}
  }

  private val refreshSequences: Endo[EngineState] = (st:EngineState) => {
    st.sequences.map{ case (id, obsseq) => updateSequenceEndo(id, obsseq) }.foldLeft(st){case (s, f) => f(s)}
  }

  private def modifyStateEvent(v: SeqEvent, svs: => SequencesQueue[SequenceView]): SeqexecEvent = v match {
    case NullSeqEvent                       => NullEvent
    case SetOperator(_, _)                  => OperatorUpdated(svs)
    case SetObserver(_, _, _)               => ObserverUpdated(svs)
    case AddLoadedSequence(i, s, _, c)      => LoadSequenceUpdated(i, s, svs, c)
    case ClearLoadedSequences(_)            => ClearLoadedSequencesUpdated(svs)
    case SetConditions(_, _)                => ConditionsUpdated(svs)
    case SetImageQuality(_, _)              => ConditionsUpdated(svs)
    case SetWaterVapor(_, _)                => ConditionsUpdated(svs)
    case SetSkyBackground(_, _)             => ConditionsUpdated(svs)
    case SetCloudCover(_, _)                => ConditionsUpdated(svs)
    case LoadSequence(id)                   => SequenceLoaded(id, svs)
    case UnloadSequence(id)                 => SequenceUnloaded(id, svs)
    case NotifyUser(m, cid)                 => UserNotification(m, cid)
    case UpdateQueueAdd(qid, seqs)          => QueueUpdated(QueueManipulationOp.AddedSeqs(qid, seqs), svs)
    case UpdateQueueRemove(qid, s, p)       => QueueUpdated(QueueManipulationOp.RemovedSeqs(qid, s, p), svs)
    case UpdateQueueMoved(qid, cid, oid, p) => QueueUpdated(QueueManipulationOp.Moved(qid, cid, oid, p), svs)
    case UpdateQueueClear(qid)              => QueueUpdated(QueueManipulationOp.Clear(qid), svs)
    case StartQueue(qid, _)                 => QueueUpdated(QueueManipulationOp.Started(qid), svs)
    case StopQueue(qid, _)                  => QueueUpdated(QueueManipulationOp.Stopped(qid), svs)
    case StartSysConfig(_, _, _)            => NullEvent
  }

  def toSeqexecEvent(ev: executeEngine.ResultType)(svs: => SequencesQueue[SequenceView]): SeqexecEvent = ev match {
    case engine.UserCommandResponse(ue, _, uev) => ue match {
      case engine.Start(_, _, _, _)      => SequenceStart(svs)
      case engine.Pause(_, _)            => SequencePauseRequested(svs)
      case engine.CancelPause(_, _)      => SequencePauseCanceled(svs)
      case engine.Breakpoint(_, _, _, _) => StepBreakpointChanged(svs)
      case engine.SkipMark(_, _, _, _)   => StepSkipMarkChanged(svs)
      case engine.Poll(cid)              => SequenceRefreshed(svs, cid)
      case engine.GetState(_)            => NullEvent
      case engine.ModifyState(_)         => modifyStateEvent(uev.getOrElse(NullSeqEvent), svs)
      case engine.ActionStop(_, _)       => ActionStopRequested(svs)
      case engine.LogDebug(_)            => NullEvent
      case engine.LogInfo(_)             => NullEvent
      case engine.LogWarning(_)          => NullEvent
      case engine.LogError(_)            => NullEvent
      case engine.ActionResume(_, _, _)  => SequenceUpdated(svs)
    }
    case engine.SystemUpdate(se, _)             => se match {
      // TODO: Sequence completed event not emited by engine.
      case engine.Completed(_, _, _, _)                                    => SequenceUpdated(svs)
      case engine.PartialResult(i, s, _, Partial(Progress(t, r)))          =>
        ObservationProgressEvent(ObservationProgress(i, s, t, r.self))
      case engine.PartialResult(_, _, _, Partial(FileIdAllocated(fileId))) =>
        FileIdStepExecuted(fileId, svs)
      case engine.PartialResult(_, _, _, _)                                => SequenceUpdated(svs)
      case engine.Failed(id, _, _)                                         => SequenceError(id, svs)
      case engine.Busy(id, clientId)                                       =>
        UserNotification(ResourceConflict(id), clientId)
      case engine.Executed(s)                                              => StepExecuted(s, svs)
      case engine.Executing(_)                                             => SequenceUpdated(svs)
      case engine.Finished(_)                                              => SequenceCompleted(svs)
      case engine.Null                                                     => NullEvent
      case engine.Paused(id, _, _)                                         => ExposurePaused(id,
        svs)
      case engine.BreakpointReached(id)                                    => SequencePaused(id,
        svs)
      case engine.SingleRunCompleted(_, _)                                 => NullEvent
      case engine.SingleRunFailed(_, _)                                    => NullEvent
    }
  }

  implicit private final class WithEvent(val f: Endo[EngineState]) extends AnyVal {
    def withEvent(ev: SeqEvent): EngineState => (EngineState, SeqEvent) = f >>> {(_, ev)}
  }
  // scalastyle:on

}
