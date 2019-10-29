// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats._
import cats.data.StateT
import cats.effect.{ConcurrentEffect, ContextShift, IO, Sync, Timer}
import cats.implicits._
import edu.gemini.seqexec.odb.SeqFailure
import edu.gemini.epics.acm.CaService
import fs2.{Pure, Stream}
import gem.Observation
import gem.enum.Site
import giapi.client.ghost.GhostClient
import giapi.client.gpi.GpiClient
import io.chrisdavenport.log4cats.Logger
import java.util.concurrent.TimeUnit
import mouse.all._
import monocle.Monocle._
import monocle.Optional
import seqexec.engine.Result.Partial
import seqexec.engine.EventResult._
import seqexec.engine.{Step => _, _}
import seqexec.engine.Handle
import seqexec.engine.SystemEvent
import seqexec.engine.UserEvent
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.events.{SequenceStart => ClientSequenceStart, _}
import seqexec.model.{StepId, UserDetails}
import seqexec.model.config._
import seqexec.server.gpi.GpiStatusApply
import seqexec.server.SeqEvent._
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

class SeqexecEngine private [server] (
  val systems: Systems[IO],
  settings: SeqexecEngineConfiguration,
  sm: SeqexecMetrics,
  translator: SeqTranslate
)(
  implicit ceio: ConcurrentEffect[IO], tio: Timer[IO]
) {
  import SeqexecEngine._

  private val odbLoader = new ODBSequencesLoader(systems.odb, translator)

  def sync(q: EventQueue[IO], seqId: Observation.Id): IO[Either[SeqexecFailure, Unit]] =
    odbLoader.loadEvents(seqId).flatMap{ e =>
      q.enqueue(
        Stream.emits(e)).map(_.asRight).compile.last.attempt.map(_.bimap(SeqexecFailure.SeqexecException.apply, _ => ())
      )
    }

  private def checkResources(seqId: Observation.Id)(st: EngineState): Boolean = {
    // Resources used by running sequences
    val used = resourcesInUse(st)

    // Resources that will be used by sequences in running queues
    val reservedByQueues = resourcesReserved(st)

    st.sequences.get(seqId).exists(x =>
      x.seqGen.resources.intersect(used).isEmpty && (
        st.queues.values.filter(_.status(st).running).exists(_.queue.contains(seqId)) ||
        x.seqGen.resources.intersect(reservedByQueues).isEmpty
      )
    )
  }

  def start[F[_]: Functor](q: EventQueue[F], id: Observation.Id, user: UserDetails, clientId: ClientId): F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.start[IO, executeEngine.ConcreteTypes](id, user, clientId, checkResources(id))).map(_.asRight)

  def startFrom[F[_]: Functor](q: EventQueue[F], id: Observation.Id, stp: StepId, clientId: ClientId): F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.modifyState[IO, executeEngine.ConcreteTypes](
      executeEngine.get.flatMap(st => checkResources(id)(st).fold(
        executeEngine.startFrom(id, stp).as(SequenceStart(id, stp)),
        executeEngine.unit.as(Busy(id, clientId))
      ) )
    ) ).map(_.asRight)

  def requestPause[F[_]: Functor](q: EventQueue[F], id: Observation.Id, user: UserDetails): F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.pause(id, user)).map(_.asRight)

  def requestCancelPause[F[_]: Functor](q: EventQueue[F], id: Observation.Id, user: UserDetails): F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.cancelPause(id, user)).map(_.asRight)

  def setBreakpoint[F[_]: Functor](q: EventQueue[F],
                    seqId: Observation.Id,
                    user: UserDetails,
                    stepId: StepId,
                    v: Boolean): F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.breakpoint(seqId, user, stepId, v)).map(_.asRight)

  def setOperator[F[_]: FlatMap](q: EventQueue[F], user: UserDetails, name: Operator): F[Either[SeqexecFailure,
    Unit]] = q.enqueue1(Event.logDebugMsg(s"SeqexecEngine: Setting Operator name to '$name' by " +
    s"${user.username}")) *> q.enqueue1(Event.modifyState[IO, executeEngine.ConcreteTypes](
    (EngineState.operator.set(name.some) >>> refreshSequences withEvent SetOperator(name,
      user.some)).toHandle)).map(_.asRight)

  def setObserver[F[_]: FlatMap](q: EventQueue[F],
                  seqId: Observation.Id,
                  user: UserDetails,
                  name: Observer): F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg(s"SeqexecEngine: Setting Observer name to '$name' for sequence '${seqId.format}' by ${user.username}")) *>
        q.enqueue1(Event.modifyState[IO, executeEngine.ConcreteTypes](
          ((EngineState.sequences ^|-? index(seqId)).modify(SequenceData.observer.set(name.some)) >>> refreshSequence(seqId) withEvent SetObserver(seqId, user.some, name)).toHandle)).map(_.asRight)

  def selectSequenceEvent(i: Instrument, sid: Observation.Id, observer: Observer, user: UserDetails, clientId: ClientId): executeEngine.EventType = {
    val lens =
      (EngineState.sequences ^|-? index(sid)).modify(SequenceData.observer.set(observer.some)) >>>
       EngineState.instrumentLoadedL(i).set(sid.some) >>>
       refreshSequence(sid)
    def testRunning(st: EngineState):Boolean = (for {
      sels   <- st.selected.get(i)
      obsseq <- st.sequences.get(sels)
    } yield obsseq.seq.status.isRunning).getOrElse(false)

    Event.modifyState[IO, executeEngine.ConcreteTypes]{ ((st: EngineState) => {
      if (!testRunning(st)) (lens withEvent AddLoadedSequence(i, sid, user, clientId))(st)
      else (st, NotifyUser(InstrumentInUse(sid, i), clientId))
    }).toHandle }
  }

  def selectSequence(q: EventQueue[IO], i: Instrument, sid: Observation.Id, observer: Observer, user: UserDetails, clientId: ClientId): IO[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logInfoMsg(s"User '${user.displayName}' sync and load sequence ${sid.format} on ${i.show}")) *>
    sync(q, sid) *>
    q.enqueue1(selectSequenceEvent(i, sid, observer, user, clientId)).map(_.asRight)

  def clearLoadedSequences[F[_]: FlatMap](q: EventQueue[F], user: UserDetails): F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Updating loaded sequences")) *>
    q.enqueue1(Event.modifyState[IO, executeEngine.ConcreteTypes]((EngineState.selected.set(Map.empty) withEvent ClearLoadedSequences(user.some)).toHandle)).map(_.asRight)

  def setConditions[F[_]: FlatMap](q: EventQueue[F], conditions: Conditions, user: UserDetails): F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting conditions")) *>
    q.enqueue1(Event.modifyState[IO, executeEngine.ConcreteTypes]((EngineState.conditions.set(conditions) >>> refreshSequences withEvent SetConditions(conditions, user.some)).toHandle)).map(_.asRight)

  def setImageQuality[F[_]: FlatMap](q: EventQueue[F], iq: ImageQuality, user: UserDetails): F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting image quality")) *>
    q.enqueue1(Event.modifyState[IO, executeEngine.ConcreteTypes](((EngineState.conditions ^|-> Conditions.iq).set(iq) >>> refreshSequences withEvent SetImageQuality(iq, user.some)).toHandle)).map(_.asRight)

  def setWaterVapor[F[_]: FlatMap](q: EventQueue[F], wv: WaterVapor, user: UserDetails): F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting water vapor")) *>
    q.enqueue1(Event.modifyState[IO, executeEngine.ConcreteTypes](((EngineState.conditions ^|-> Conditions.wv).set(wv) >>> refreshSequences withEvent SetWaterVapor(wv, user.some)).toHandle)).map(_.asRight)

  def setSkyBackground[F[_]: FlatMap](q: EventQueue[F], sb: SkyBackground, user: UserDetails): F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting sky background")) *>
    q.enqueue1(Event.modifyState[IO, executeEngine.ConcreteTypes](((EngineState.conditions ^|-> Conditions.sb).set(sb) >>> refreshSequences withEvent SetSkyBackground(sb, user.some)).toHandle)).map(_.asRight)

  def setCloudCover[F[_]: FlatMap](q: EventQueue[F], cc: CloudCover, user: UserDetails): F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.logDebugMsg("SeqexecEngine: Setting cloud cover")) *>
    q.enqueue1(Event.modifyState[IO, executeEngine.ConcreteTypes](((EngineState.conditions ^|-> Conditions.cc).set(cc) >>> refreshSequences withEvent SetCloudCover(cc, user.some)).toHandle)).map(_.asRight)

  def setSkipMark[F[_]: Functor](q: EventQueue[F],
                  seqId: Observation.Id,
                  user: UserDetails,
                  stepId: StepId,
                  v: Boolean): F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(Event.skip(seqId, user, stepId, v)).map(_.asRight)

  def requestRefresh[F[_]](q: EventQueue[F], clientId: ClientId): F[Unit] = q.enqueue1(Event.poll(clientId))

  def seqQueueRefreshStream: Stream[IO, Either[SeqexecFailure, executeEngine.EventType]] = {
    val fd = Duration(settings.odbQueuePollingInterval.toSeconds, TimeUnit.SECONDS)
    Stream.fixedDelay[IO](fd).evalMap(_ => systems.odb.queuedSequences).flatMap { x =>
      Stream.emit(Event.getState[IO, executeEngine.ConcreteTypes] { st =>
        Stream.eval(odbLoader.refreshSequenceList(x, st)).flatMap(Stream.emits).some
      }.asRight)
    }.handleErrorWith {
      case e: SeqFailure =>
        Stream.emit(SeqexecFailure.OdbSeqError(e).asLeft)
      case e: Exception =>
        Stream.emit(SeqexecFailure.SeqexecException(e).asLeft)
    }
  }

  def eventStream(q: EventQueue[IO]): Stream[IO, SeqexecEvent] =
    stream(q.dequeue.mergeHaltBoth(seqQueueRefreshStream.rethrow))(EngineState.default).flatMap(x =>
      Stream.eval(notifyODB(x))).flatMap {
        case (ev, qState) =>
          val sequences = qState.sequences.values.map(viewSequence).toList
          val event = toSeqexecEvent(ev, qState)
          Stream.eval(updateMetrics[IO](ev, sequences).as(event))
    }

  private[server] def stream(p: Stream[IO, executeEngine.EventType])(s0: EngineState)
  : Stream[IO, (executeEngine.ResultType, EngineState)] =
    executeEngine.process(iterateQueues)(p)(s0)

  def stopObserve[F[_]](q: EventQueue[F], seqId: Observation.Id, graceful: Boolean): F[Unit] = q.enqueue1(
    Event.actionStop[IO, executeEngine.ConcreteTypes](seqId, translator.stopObserve(seqId, graceful))
  )

  def abortObserve[F[_]](q: EventQueue[F], seqId: Observation.Id, graceful: Boolean): F[Unit] = q.enqueue1(
    Event.actionStop[IO, executeEngine.ConcreteTypes](seqId, translator.abortObserve(seqId, graceful))
  )

  def pauseObserve[F[_]](q: EventQueue[F], seqId: Observation.Id, graceful: Boolean): F[Unit] = q.enqueue1(
    Event.actionStop[IO, executeEngine.ConcreteTypes](seqId, translator.pauseObserve(seqId, graceful))
  )

  def resumeObserve[F[_]](q: EventQueue[F], seqId: Observation.Id): F[Unit] = q.enqueue1(
    Event.getState[IO, executeEngine.ConcreteTypes](translator.resumePaused(seqId))
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
          case (BatchCommandState.Run(o, u, c), _) => executeEngine.get.flatMap(st2 => runSequences(shouldSchedule(qid,
            seqs.toSet)(st2), o, u, c))
          case _                                   => executeEngine.unit
        })
    ).getOrElse(executeEngine.unit)}

  def addSequencesToQueue[F[_]: Functor](q: EventQueue[F], qid: QueueId, seqIds: List[Observation.Id])
  : F[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[IO, executeEngine.ConcreteTypes](addSeqs(qid, seqIds)
      .as[executeEngine.ConcreteTypes#EventData](UpdateQueueAdd(qid, seqIds)))
  ).map(_.asRight)

  def addSequenceToQueue(q: EventQueue[IO], qid: QueueId, seqId: Observation.Id): IO[Either[SeqexecFailure, Unit]] =
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
          case (_, BatchExecState.Completed)       => executeEngine.unit
          // If removed sequence was halting the queue, then removing it frees resources to run the next sequences
          case (BatchCommandState.Run(o, u, c), _) => shouldSchedule(qid, Set(seqId))(st).isEmpty.fold(
            executeEngine.unit,
            st.sequences.get(seqId).map(x => runNextsInQueue(qid, o, u, c, x.seqGen.resources))
              .getOrElse(executeEngine.unit)
          )
          case _                                   => executeEngine.unit
        })
    ).getOrElse(executeEngine.unit)}

  def removeSequenceFromQueue[F[_]: Functor](q: EventQueue[F], qid: QueueId, seqId: Observation.Id)
  : F[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[IO, executeEngine.ConcreteTypes](
      executeEngine.get.flatMap(st => removeSeq(qid, seqId)
        .as(UpdateQueueRemove(qid, List(seqId), st.queues.get(qid)
          .map(_.queue.indexOf(seqId)).toList))))
  ).map(_.asRight)

  private def moveSeq(qid: QueueId, seqId: Observation.Id, delta: Int): Endo[EngineState] = st =>
    st.queues.get(qid).filter(_.queue.contains(seqId)).map {_ =>
      queueO(qid).modify(_.moveSeq(seqId, delta))(st)
    }.getOrElse(st)

  def moveSequenceInQueue[F[_]: Functor](q: EventQueue[F], qid: QueueId, seqId: Observation.Id, delta: Int, cid: ClientId)
  : F[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[IO, executeEngine.ConcreteTypes](
      executeEngine.get.flatMap(_ => (moveSeq(qid, seqId, delta) withEvent UpdateQueueMoved(qid,
        cid, seqId, 0)).toHandle))
    ).map(_.asRight)

  private def clearQ(qid: QueueId): Endo[EngineState] = st =>
    st.queues.get(qid).filter(_.status(st) =!= BatchExecState.Running).map { _ =>
      queueO(qid).modify(_.clear)(st)
    }.getOrElse(st)

  def clearQueue[F[_]: Functor](q: EventQueue[F], qid: QueueId): F[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[IO, executeEngine.ConcreteTypes](
      (clearQ(qid) withEvent UpdateQueueClear(qid)).toHandle)
  ).map(_.asRight)

  private def setObserverAndSelect(sid: Observation.Id, observer: Observer, user: UserDetails, clientId: ClientId)
  : executeEngine.HandleType[Unit] = Handle(
    StateT[IO, EngineState,(Unit, Option[Stream[IO, executeEngine.EventType]])] {
      st:EngineState => IO(
        (EngineState.sequences ^|-? index(sid)).getOption(st).map{ obsseq =>
          (EngineState.sequences.modify(_ + (sid -> obsseq.copy(observer = observer.some))) >>>
            refreshSequence(sid) >>>
            EngineState.instrumentLoadedL(obsseq.seqGen.instrument).set(sid.some) >>>
            {(_, ((), Stream[Pure, executeEngine.EventType](
              Event.modifyState[IO, executeEngine.ConcreteTypes](
                { {s:EngineState => s} withEvent
                  AddLoadedSequence(obsseq.seqGen.instrument, sid, user, clientId)
                }.toHandle
              )
            ).covary[IO].some))}
          )(st)
        }.getOrElse((st, ((), None)))
      )
    }
  )

  private def runSequences(ss: Set[Observation.Id], observer: Observer, user: UserDetails, clientId: ClientId)
  :executeEngine.HandleType[Unit] =
    ss.map(sid => setObserverAndSelect(sid, observer, user, clientId) *> executeEngine.start(sid, clientId,
      { _ =>true }
    )).fold(executeEngine.unit)(_ *> _)

  /**
   * Most of the magic for the ExecutionQueue is done in the following functions.
   */

  /**
   * runQueue starts the queue. It founds the top eligible sequences in the queue, and runs them.
   */
  private def runQueue(qid: QueueId, observer: Observer, user: UserDetails, clientId: ClientId)
  : executeEngine.HandleType[Unit] = {

    executeEngine.get.map(findRunnableObservations(qid)).flatMap(runSequences(_, observer, user, clientId))
  }

  /**
   * runNextsInQueue continues running the queue after a sequence completes. It founds the next eligible sequences in
   * the queue, and runs them.
   * At any given time a queue can be running, but one of the top eligible sequences are not. That is the case if the
   * sequence ended with an error or is stopped by the user. In both cases, the sequence should not be restarted
   * without user intervention, nor other sequence that uses the same resources should be started. Because of that,
   * runNextsInQueue only runs sequences that are now eligible because of the resources that the just completed
   * sequence has freed.
   */
  private def runNextsInQueue(qid: QueueId, observer: Observer, user: UserDetails, clientId: ClientId,
                              freed: Set[Resource]): executeEngine.HandleType[Unit] = {
    executeEngine.get.map(nextRunnableObservations(qid, freed)).flatMap(runSequences(_, observer, user, clientId))
  }

  def startQueue[F[_]: Functor](q: EventQueue[F], qid: QueueId, observer: Observer, user: UserDetails, clientId: ClientId)
  : F[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[IO, executeEngine.ConcreteTypes](executeEngine.get.flatMap{ st => {
      queueO(qid).getOption(st).filterNot(_.queue.isEmpty).map {
        _.status(st) match {
          case BatchExecState.Idle |
               BatchExecState.Stopping => ((EngineState.queues ^|-? index(qid) ^|-> ExecutionQueue.cmdState)
            .set(BatchCommandState.Run(observer, user, clientId)) >>> {(_, ())}).toHandle *>
            runQueue(qid, observer, user, clientId)
          case _                       => executeEngine.unit
        }
      }.getOrElse(executeEngine.unit)
    }}.as(StartQueue(qid, clientId)))
  ).map(_.asRight)

  private def stopSequencesInQueue(qid: QueueId): executeEngine.HandleType[Unit] =
    executeEngine.get.map(st =>
      queueO(qid).getOption(st)
        .foldMap(_.queue.filter(sid => EngineState.sequenceStateIndex(sid)
          .getOption(st).exists(_.status.isRunning)))
    ).flatMap(_.map(executeEngine.pause).fold(executeEngine.unit)(_ *> _))

  def stopQueue[F[_]: Functor](q: EventQueue[F], qid: QueueId, clientId: ClientId): F[Either[SeqexecFailure, Unit]] = q.enqueue1(
    Event.modifyState[IO, executeEngine.ConcreteTypes](executeEngine.get.flatMap{ st =>
      queueO(qid).getOption(st).map {
        _.status(st) match {
          case BatchExecState.Running => (cmdStateO(qid).set(BatchCommandState.Stop) >>> {(_, ())}).toHandle *>
            stopSequencesInQueue(qid)
          case BatchExecState.Waiting => (cmdStateO(qid).set(BatchCommandState.Stop) >>> {(_, ())}).toHandle
          case _                      => executeEngine.unit
        }
      }.getOrElse(executeEngine.unit)
    }.as(StopQueue(qid, clientId)))
  ).map(_.asRight)

  private val iterateQueues: PartialFunction[SystemEvent[IO], executeEngine.HandleType[Unit]] = {
    // Events that could trigger the scheduling of the next sequence in the queue:
    // - The completion of a sequence (and subsequent release of resources)
    case SystemEvent.Finished(sid)               => executeEngine.get.flatMap(st =>
      st.sequences.get(sid).flatMap { seq =>
        val freed = seq.seqGen.resources
        st.queues.collectFirst {
          case (qid, q@ExecutionQueue(_, BatchCommandState.Run(observer, user, clid), _))
            if q.status(st) =!= BatchExecState.Completed =>
            runNextsInQueue(qid, observer, user, clid, freed)
        }
      }.getOrElse(executeEngine.unit)
    )
  }

  private def configSystemCheck(sid: Observation.Id, sys: Resource)(st: EngineState): Boolean = {
    // Resources used by running sequences
    val used = resourcesInUse(st)

    // Resources reserved by running queues, excluding `sid` to prevent self blocking
    val reservedByQueues = resourcesReserved(EngineState.sequences.modify(_ - sid)(st))

    !(used ++ reservedByQueues).contains(sys)
  }

  private def configSystemHandle(sid: Observation.Id, stepId: StepId, sys: Resource, clientID: ClientId)
  : executeEngine.HandleType[SeqEvent] = {

    executeEngine.get.flatMap{ st =>
      if (configSystemCheck(sid, sys)(st)) {
        st.sequences.get(sid).flatMap(_.seqGen.configActionCoord(stepId, sys))
          .map(c => executeEngine.startSingle(ActionCoords(sid, c)).map[SeqEvent]{
            case EventResult.Outcome.Ok => StartSysConfig(sid, stepId, sys)
            case _                      => NullSeqEvent
          }).getOrElse(executeEngine.pure(NullSeqEvent))
      } else {
        executeEngine.pure(ResourceBusy(sid, stepId, sys, clientID))
      }
    }
  }

  /**
   *  Triggers the application of a specific step configuration to a system
   */
  def configSystem[F[_]: Functor](q: EventQueue[F], sid: Observation.Id, stepId: StepId, sys: Resource, clientID: ClientId)
  : F[Either[SeqexecFailure, Unit]] =
    q.enqueue1(
      Event.modifyState[IO, executeEngine.ConcreteTypes](configSystemHandle(sid, stepId, sys, clientID))
    ).map(_.asRight)

  def notifyODB(i: (executeEngine.ResultType, EngineState)): IO[(executeEngine.ResultType, EngineState)] = {
    (i match {
      case (SystemUpdate(SystemEvent.Failed(id, _, e), _), _) =>
        systems.odb.obsAbort(id, e.msg)
      case (SystemUpdate(SystemEvent.Executed(id), _), st) if EngineState.sequenceStateIndex(id).getOption(st)
        .exists(_.status === SequenceState.Idle) =>
        systems.odb.obsPause(id, "Sequence paused by user")
      case (SystemUpdate(SystemEvent.Finished(id), _), _)     => systems.odb.sequenceEnd(id)
      case _                                  => IO.unit
    }).as(i)
  }

  /**
   * Update some metrics based on the event types
   */
  def updateMetrics[F[_]: Sync](e: executeEngine.ResultType, sequences: List[SequenceView]): F[Unit] = {
    def instrument(id: Observation.Id): Option[Instrument] =
      sequences.find(_.id === id).map(_.metadata.instrument)

    (e match {
      // TODO Add metrics for more events
      case UserCommandResponse(ue, _, _)   => ue match {
        case UserEvent.Start(id, _, _, _) => instrument(id).map(sm.startRunning[F]).getOrElse(Sync[F].unit)
        case _                            => Sync[F].unit
      }
      case SystemUpdate(se, _) => se match {
        case _ => Sync[F].unit
      }
      case _                      => Sync[F].unit
    }).flatMap(_ => Sync[F].unit)
  }
}

object SeqexecEngine {

  def createTranslator(site: Site, systems: Systems[IO])(implicit L: Logger[IO]): IO[SeqTranslate] =
    SeqTranslate(site, systems)

  def splitWhere[A](l: List[A])(p: A => Boolean): (List[A], List[A]) =
    l.splitAt(l.indexWhere(p))

  private def systemsBeingConfigured(st: EngineState): Set[Resource] =
    st.sequences.values.filter(d => d.seq.status.isError || d.seq.status.isIdle).toList
      .flatMap(s => s.seq.getSingleActionStates.filter(_._2.started).keys.toList
        .mapFilter(s.seqGen.resourceAtCoords)
      ).toSet

  /**
   * Resource in use = Resources used by running sequences, plus the systems that are being configured because a user
   * commanded a manual configuration apply.
   */
  private def resourcesInUse(st: EngineState): Set[Resource] =
    st.sequences.values.toList.mapFilter(s => s.seq.status.isRunning.option(s.seqGen.resources)).foldK ++
      systemsBeingConfigured(st)

  /**
   * Resources reserved by running queues.
   */
  private def resourcesReserved(st: EngineState): Set[Resource] = {
    def reserved(q: ExecutionQueue): Set[Resource] = q.queue.fproduct(st.sequences.get).collect{
      case (_, Some(s)) if s.seq.status.isIdle => s.seqGen.resources
    }.foldK

    val runningQs = st.queues.values.filter(_.status(st).running)

    runningQs.map(reserved).toList.foldK

  }

  /**
    * Find the observations in an execution queue that would be run next, taking into account the resources required by
    * each observation and the resources currently in use.
    * The order in the queue defines the priority of the observations.
    * Failed or stopped sequences in the queue keep their instruments taken, preventing that the queue starts other
    * sequences for those instruments.
    * @param qid The execution queue id
    * @param st The current engine state
    * @return The set of all observations in the execution queue `qid` that can be started to run
    *         in parallel.
    */
  def findRunnableObservations(qid: QueueId)(st: EngineState)
  : Set[Observation.Id] = {
    // Set of all resources in use
    val used = resourcesInUse(st)
    // For each observation in the queue that is not yet run, retrieve the required resources
    val obs = st.queues.get(qid).map(_.queue.fproduct(st.sequences.get).collect {
      case (id, Some(s)) if !s.seq.status.isRunning && !s.seq.status.isCompleted =>
        id -> s.seqGen.resources
    }).orEmpty

    obs.foldLeft((used, Set.empty[Observation.Id])){
      case ((u, a),(oid, res)) => if(u.intersect(res).isEmpty)
        (u ++ res, a + oid) else (u, a)
    }._2
  }

  /**
    * Find next runable observations given that a set of resources has just being released
    * @param qid The execution queue id
    * @param st The current engine state
    * @param freed Resources that were freed
    * @return The set of all observations in the execution queue `qid` that can be started to run
    *         in parallel.
    */
  def nextRunnableObservations(qid: QueueId, freed: Set[Resource])(st: EngineState)
  : Set[Observation.Id] = {
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

    obs.foldLeft((used ++ resFailed, Set[Observation.Id]())){
      case ((u, a),(oid, res)) => if(u.intersect(res).isEmpty && freed.intersect(res).nonEmpty) (u ++ res, a + oid)
                                  else (u, a)
    }._2
  }

  /**
   * shouldSchedule checks if a set of sequences are candidates for been run in a queue.
   * It is used to check if sequences added to a queue should be started.
   */
  def shouldSchedule(qid: QueueId, sids: Set[Observation.Id])(st: EngineState): Set[Observation.Id] =
    findRunnableObservations(qid)(st).intersect(sids)

  def gpiClient(control: ControlStrategy, gpiUrl: String)(implicit cs: ContextShift[IO], t: Timer[IO]): cats.effect.Resource[IO, GpiClient[IO]] =
    if (control === ControlStrategy.FullControl) {
      GpiClient.gpiClient[IO](gpiUrl, GpiStatusApply.statusesToMonitor)
    } else {
      GpiClient.simulatedGpiClient
    }

  def ghostClient(control: ControlStrategy, ghostUrl: String)(implicit cs: ContextShift[IO], t: Timer[IO]): cats.effect.Resource[IO, GhostClient[IO]] =
    if (control === ControlStrategy.FullControl) {
      GhostClient.ghostClient[IO](ghostUrl)
    } else {
      GhostClient.simulatedGhostClient
    }

  // Ensure there is a valid way to init CaService either from
  // the configuration file or from the environment
  def caInit(caAddrList: Option[String], ioTimeout: Duration): IO[CaService] =
    caAddrList.map(a => IO.apply(CaService.setAddressList(a))).getOrElse {
      IO.apply(Option(System.getenv("EPICS_CA_ADDR_LIST"))).flatMap {
        case Some(_) => IO.unit
        case _       => IO.raiseError(new RuntimeException("Cannot initialize EPICS subsystem"))
      }
    } *>
      IO.apply(CaService.setIOTimeout(java.time.Duration.ofMillis(ioTimeout.toMillis))) *>
      IO.apply(Option(CaService.getInstance())).flatMap {
        case None => (new Exception("Unable to start EPICS service.")).raiseError[IO, CaService]
        case Some(s) => s.pure[IO]
      }

  /**
   * Build the seqexec and setup epics
   */
  def build(
    site: Site,
    systems: Systems[IO],
    conf: SeqexecEngineConfiguration,
    metrics: SeqexecMetrics)(
    implicit cs: ContextShift[IO],
             tio: Timer[IO],
             L: Logger[IO]
  ): IO[SeqexecEngine] =
    createTranslator(site, systems)
      .map{ new SeqexecEngine(systems, conf, metrics, _) }

  private[server] def updateSequenceEndo(seqId: Observation.Id, obsseq: SequenceData[IO])
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
    case StartSysConfig(sid, stepId, res)   => SingleActionEvent(SingleActionOp.Started(sid, stepId, res))
    case SequenceStart(sid, stepId)         => ClientSequenceStart(sid, stepId, svs)
    case Busy(id, cid)                      => UserNotification(ResourceConflict(id), cid)
    case ResourceBusy(id, sid, res, cid)    => UserNotification(SubsystemBusy(id, sid, res), cid)
  }

  private def executionQueueViews(st: EngineState): SortedMap[QueueId, ExecutionQueueView] = {
    SortedMap(st.queues.map {
      case (qid, q) => qid -> ExecutionQueueView(qid, q.name, q.cmdState, q.status(st), q.queue)
    }.toList: _*)
  }

  private def viewSequence[F[_]](obsSeq: SequenceData[F]): SequenceView = {
    val st = obsSeq.seq
    val seq = st.toSequence
    val instrument = obsSeq.seqGen.instrument

    def resources(s: SequenceGen.StepGen[F]): List[Resource] = s match {
      case s: SequenceGen.PendingStepGen[F] => s.resources.toList
      case _                                => List.empty
    }
    def engineSteps(seq: Sequence[F]): List[Step] = {

      obsSeq.seqGen.steps.zip(seq.steps).map{
        case (a, b) => StepsView.stepsView(instrument).stepView(a, b, resources(a).mapFilter(x =>
          obsSeq.seqGen.configActionCoord(a.id, x)
            .map(i => (x, obsSeq.seq.getSingleState(i).actionStatus))
        ))
      }
      match {
        // The sequence could be empty
        case Nil => Nil
        // Find first Pending Step when no Step is Running and mark it as Running
        case steps if Sequence.State.isRunning(st) && steps.forall(_.status =!= StepState.Running) =>
          val (xs, y :: ys) = splitWhere(steps)(_.status === StepState.Pending)
          xs ++ (Step.status.set(StepState.Running)(y) :: ys)
        case steps if st.status === SequenceState.Idle && steps.exists(_.status === StepState.Running) =>
          val (xs, y :: ys) = splitWhere(steps)(_.status === StepState.Running)
          xs ++ (Step.status.set(StepState.Paused)(y) :: ys)
        case x => x
      }
    }

    // TODO: Implement willStopIn
    SequenceView(seq.id, SequenceMetadata(instrument, obsSeq.observer, obsSeq.seqGen.title), st.status, engineSteps(seq), None)
  }

  def toSeqexecEvent(ev: executeEngine.ResultType, qState: EngineState): SeqexecEvent = {
    val sequences = qState.sequences.values.map(viewSequence).toList
    // Building the view is a relatively expensive operation
    // By putting it into a def we only incur that cost if the message requires it
    def svs =
      SequencesQueue(
      EngineState.selected.get(qState),
      EngineState.conditions.get(qState),
      EngineState.operator.get(qState),
      executionQueueViews(qState),
      sequences)

    ev match {
      case UserCommandResponse(ue, _, uev) => ue match {
        case UserEvent.Start(id, _, _, _)     =>
          val rs = sequences.find(_.id === id).flatMap(_.runningStep)
          ClientSequenceStart(id, rs.foldMap(_.last), svs)
        case UserEvent.Pause(_, _)            => SequencePauseRequested(svs)
        case UserEvent.CancelPause(id, _)     => SequencePauseCanceled(id, svs)
        case UserEvent.Breakpoint(_, _, _, _) => StepBreakpointChanged(svs)
        case UserEvent.SkipMark(_, _, _, _)   => StepSkipMarkChanged(svs)
        case UserEvent.Poll(cid)              => SequenceRefreshed(svs, cid)
        case UserEvent.GetState(_)            => NullEvent
        case UserEvent.ModifyState(_)         => modifyStateEvent(uev.getOrElse(NullSeqEvent), svs)
        case UserEvent.ActionStop(_, _)       => ActionStopRequested(svs)
        case UserEvent.LogDebug(_)            => NullEvent
        case UserEvent.LogInfo(_)             => NullEvent
        case UserEvent.LogWarning(_)          => NullEvent
        case UserEvent.LogError(_)            => NullEvent
        case UserEvent.ActionResume(_, _, _)  => SequenceUpdated(svs)
      }
      case SystemUpdate(se, _)             => se match {
        // TODO: Sequence completed event not emitted by engine.
        case SystemEvent.Completed(_, _, _, _)                                    => SequenceUpdated(svs)
        case SystemEvent.StopCompleted(id, _, _, _)                               => SequenceStopped(id, svs)
        case SystemEvent.PartialResult(_, _, _, Partial(_: InternalPartialVal))   => NullEvent
        case SystemEvent.PartialResult(i, s, _, Partial(ObsProgress(t, r)))          =>
          ObservationProgressEvent(ObservationProgress(i, s, t, r.self))
        case SystemEvent.PartialResult(i, s, _, Partial(NSProgress(t, r, u)))          =>
          ObservationProgressEvent(NSObservationProgress(i, s, t, r.self, u))
        case SystemEvent.PartialResult(_, _, _, Partial(FileIdAllocated(fileId))) =>
          FileIdStepExecuted(fileId, svs)
        case SystemEvent.PartialResult(_, _, _, _)                                =>
          SequenceUpdated(svs)
        case SystemEvent.Failed(id, _, _)                                         => SequenceError(id, svs)
        case SystemEvent.Busy(id, clientId)                                       =>
          UserNotification(ResourceConflict(id), clientId)
        case SystemEvent.Executed(s)                                              => StepExecuted(s, svs)
        case SystemEvent.Executing(_)                                             => SequenceUpdated(svs)
        case SystemEvent.Finished(_)                                              => SequenceCompleted(svs)
        case SystemEvent.Null                                                     => NullEvent
        case SystemEvent.Paused(id, _, _)                                         => ExposurePaused(id,
          svs)
        case SystemEvent.BreakpointReached(id)                                    => SequencePaused(id,
          svs)
        case SystemEvent.SingleRunCompleted(c, _) =>
          singleActionEvent[SingleActionOp.Completed](c, qState, SingleActionOp.Completed)
        case SystemEvent.SingleRunFailed(c, r)   =>
          singleActionEvent[SingleActionOp.Error](c, qState, SingleActionOp.Error.apply(_, _, _, r.msg))
      }
    }
  }

  private def singleActionEvent[S <: SingleActionOp](c: ActionCoords,
                                                     qState: EngineState,
                                                     f: (Observation.Id, StepId, Resource) => S): SeqexecEvent =
    qState.sequences.get(c.sid).flatMap(_.seqGen.resourceAtCoords(c.actCoords))
      .map(res => SingleActionEvent(f(c.sid, c.actCoords.stepId, res)))
      .getOrElse(NullEvent)

}
