// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats._
import cats.data.StateT
import cats.effect.{Concurrent, ConcurrentEffect, Sync, Timer}
import cats.effect.concurrent.Ref
import cats.implicits._
import edu.gemini.seqexec.odb.SeqFailure
import fs2.{Pure, Stream, Pipe}
import gem.Observation
import gem.enum.Site
import io.chrisdavenport.log4cats.Logger
import java.util.concurrent.TimeUnit
import java.time.Instant
import mouse.all._
import monocle.Monocle._
import monocle.Optional
import seqexec.engine.Result.Partial
import seqexec.engine.EventResult._
import seqexec.engine.{Step => _, _}
import seqexec.engine.Handle
import seqexec.engine.SystemEvent
import seqexec.engine.UserEvent
import seqexec.model.NodAndShuffleStep.{PauseGracefully, PendingObserveCmd, StopGracefully}
import seqexec.model._
import seqexec.model.enum._
import seqexec.model.events.{SequenceStart => ClientSequenceStart, _}
import seqexec.model.{StepId, UserDetails}
import seqexec.model.config._
import seqexec.server.SeqEvent._
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

trait SeqexecEngine[F[_]] {

  val systems: Systems[F]

  def sync(q: EventQueue[F], seqId: Observation.Id): F[Unit]

  def start(q: EventQueue[F], id: Observation.Id, user: UserDetails, clientId: ClientId): F[Unit]

  def startFrom(q: EventQueue[F], id: Observation.Id, stp: StepId, clientId: ClientId): F[Unit]

  def requestPause(q: EventQueue[F], id: Observation.Id, user: UserDetails): F[Unit]

  def requestCancelPause(q: EventQueue[F], id: Observation.Id, user: UserDetails): F[Unit]

  def setBreakpoint(q: EventQueue[F],
                    seqId: Observation.Id,
                    user: UserDetails,
                    stepId: StepId,
                    v: Boolean): F[Unit]

  def setOperator(q: EventQueue[F], user: UserDetails, name: Operator): F[Unit]

  def setObserver(q: EventQueue[F],
                  seqId: Observation.Id,
                  user: UserDetails,
                  name: Observer): F[Unit]

  def selectSequence(q: EventQueue[F], i: Instrument, sid: Observation.Id, observer: Observer, user: UserDetails, clientId: ClientId): F[Unit]

  def clearLoadedSequences(q: EventQueue[F], user: UserDetails): F[Unit]

  def setConditions(q: EventQueue[F], conditions: Conditions, user: UserDetails): F[Unit]

  def setImageQuality(q: EventQueue[F], iq: ImageQuality, user: UserDetails): F[Unit]

  def setWaterVapor(q: EventQueue[F], wv: WaterVapor, user: UserDetails): F[Unit]

  def setSkyBackground(q: EventQueue[F], sb: SkyBackground, user: UserDetails): F[Unit]

  def setCloudCover(q: EventQueue[F], cc: CloudCover, user: UserDetails): F[Unit]

  def setSkipMark(q: EventQueue[F],
                  seqId: Observation.Id,
                  user: UserDetails,
                  stepId: StepId,
                  v: Boolean): F[Unit]

  def requestRefresh(q: EventQueue[F], clientId: ClientId): F[Unit]

  def stopObserve(q: EventQueue[F], seqId: Observation.Id, graceful: Boolean): F[Unit]

  def abortObserve(q: EventQueue[F], seqId: Observation.Id): F[Unit]

  def pauseObserve(q: EventQueue[F], seqId: Observation.Id, graceful: Boolean): F[Unit]

  def resumeObserve(q: EventQueue[F], seqId: Observation.Id): F[Unit]

  def addSequencesToQueue(q: EventQueue[F], qid: QueueId, seqIds: List[Observation.Id]): F[Unit]

  def addSequenceToQueue(q: EventQueue[F], qid: QueueId, seqId: Observation.Id): F[Unit]

  def removeSequenceFromQueue(q: EventQueue[F], qid: QueueId, seqId: Observation.Id): F[Unit]

  def moveSequenceInQueue(q: EventQueue[F], qid: QueueId, seqId: Observation.Id, delta: Int, cid: ClientId): F[Unit]

  def clearQueue(q: EventQueue[F], qid: QueueId): F[Unit]

  def startQueue(q: EventQueue[F], qid: QueueId, observer: Observer, user: UserDetails, clientId: ClientId): F[Unit]

  def stopQueue(q: EventQueue[F], qid: QueueId, clientId: ClientId): F[Unit]

  /**
   *  Triggers the application of a specific step configuration to a system
   */
  def configSystem(q: EventQueue[F], sid: Observation.Id, stepId: StepId, sys: Resource, clientID: ClientId): F[Unit]

  def eventStream(q: EventQueue[F]): Stream[F, SeqexecEvent]

  // Used by tests
  def stream(p: Stream[F, EventType[F]])(s0: EngineState[F])
      : Stream[F, (EventResult[SeqEvent], EngineState[F])]
}

object SeqexecEngine {

  private class SeqexecEngineImpl[F[_]: ConcurrentEffect: Timer: Logger](
    override val systems: Systems[F],
    settings: SeqexecEngineConfiguration,
    sm: SeqexecMetrics,
    translator: SeqTranslate[F]
  )(
    implicit executeEngine: seqexec.server.ExecEngineType[F]
  ) extends SeqexecEngine[F] {

    private val odbLoader = new ODBSequencesLoader[F](systems.odb, translator)

    override def sync(q: EventQueue[F], seqId: Observation.Id): F[Unit] =
      odbLoader.loadEvents(seqId).flatMap{ e =>
        q.enqueue(
          Stream.emits(e)
        ).compile.drain
      }

    private def checkResources(seqId: Observation.Id)(st: EngineState[F]): Boolean = {
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

    private def clearObsCmd(id: Observation.Id): HandleType[F, SeqEvent] = { (s: EngineState[F]) =>
      ((EngineState.atSequence[F](id) ^|-> SequenceData.pendingObsCmd).set(None)(s), SeqEvent.NullSeqEvent:SeqEvent)
    }.toHandle

    private def setObsCmd(id: Observation.Id, cmd: PendingObserveCmd): HandleType[F, SeqEvent] = { (s: EngineState[F]) =>
      ((EngineState.atSequence[F](id) ^|-> SequenceData.pendingObsCmd).set(cmd.some)(s), SeqEvent.NullSeqEvent:SeqEvent)
    }.toHandle

    override def start(q: EventQueue[F], id: Observation.Id, user: UserDetails, clientId: ClientId): F[Unit] =
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](clearObsCmd(id))) *>
        q.enqueue1(Event.start[F, EngineState[F], SeqEvent](id, user, clientId, checkResources(id))).map(_.asRight)

    override def startFrom(q: EventQueue[F], id: Observation.Id, stp: StepId, clientId: ClientId): F[Unit] =
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](
        clearObsCmd(id) *>
        executeEngine.get.flatMap(st => checkResources(id)(st).fold(
          executeEngine.startFrom(id, stp).as(SequenceStart(id, stp)),
          executeEngine.unit.as(Busy(id, clientId))
        ) )
      ) ).map(_.asRight)

    override def requestPause(q: EventQueue[F], id: Observation.Id, user: UserDetails): F[Unit] =
      q.enqueue1(Event.pause[F, EngineState[F], SeqEvent](id, user)).map(_.asRight)

    override def requestCancelPause(q: EventQueue[F], id: Observation.Id, user: UserDetails): F[Unit] =
      q.enqueue1(Event.cancelPause[F, EngineState[F], SeqEvent](id, user)).map(_.asRight)

    override def setBreakpoint(q: EventQueue[F],
                      seqId: Observation.Id,
                      user: UserDetails,
                      stepId: StepId,
                      v: Boolean): F[Unit] =
      q.enqueue1(Event.breakpoint[F, EngineState[F], SeqEvent](seqId, user, stepId, v)).map(_.asRight)

    override def setOperator(q: EventQueue[F], user: UserDetails, name: Operator): F[Unit] =
      logDebugEvent(q, s"SeqexecEngine: Setting Operator name to '$name' by ${user.username}") *>
        q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](
        (EngineState.operator[F].set(name.some) >>> refreshSequences withEvent SetOperator(name,
          user.some)).toHandle))

    override def setObserver(q: EventQueue[F],
                    seqId: Observation.Id,
                    user: UserDetails,
                    name: Observer): F[Unit] =
      logDebugEvent(q, s"SeqexecEngine: Setting Observer name to '$name' for sequence '${seqId.format}' by ${user.username}") *>
          q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](
            ((EngineState.sequences[F] ^|-? index(seqId)).modify(SequenceData.observer.set(name.some)) >>>
              refreshSequence(seqId) withEvent SetObserver(seqId, user.some, name)).toHandle))

    private def selectSequenceEvent(i: Instrument, sid: Observation.Id, observer: Observer, user: UserDetails, clientId: ClientId)
    : EventType[F] = {
      val lens =
        (EngineState.sequences[F] ^|-? index(sid)).modify(SequenceData.observer.set(observer.some)) >>>
         EngineState.instrumentLoadedL[F](i).set(sid.some) >>>
         refreshSequence(sid)
      def testRunning(st: EngineState[F]):Boolean = (for {
        sels   <- st.selected.get(i)
        obsseq <- st.sequences.get(sels)
      } yield obsseq.seq.status.isRunning).getOrElse(false)

      Event.modifyState[F, EngineState[F], SeqEvent]{ ((st: EngineState[F]) => {
        if (!testRunning(st)) (lens withEvent AddLoadedSequence(i, sid, user, clientId))(st)
        else (st, NotifyUser(InstrumentInUse(sid, i), clientId))
      }).toHandle }
    }

    override def selectSequence(q: EventQueue[F], i: Instrument, sid: Observation.Id, observer: Observer, user: UserDetails, clientId: ClientId): F[Unit] =
      Sync[F].delay(Instant.now)
        .flatMap{ts =>
          q.enqueue1(
            Event.logInfoMsg[F, EngineState[F], SeqEvent](s"User '${user.displayName}' sync and load sequence ${sid.format} on ${i.show}", ts))
          } *>
      sync(q, sid) *>
      q.enqueue1(selectSequenceEvent(i, sid, observer, user, clientId))

    private def logDebugEvent(q: EventQueue[F], msg: String): F[Unit] =
      Event.logDebugMsgF[F, EngineState[F], SeqEvent](msg).flatMap(q.enqueue1(_))

    override def clearLoadedSequences(q: EventQueue[F], user: UserDetails): F[Unit] =
      logDebugEvent(q, "SeqexecEngine: Updating loaded sequences") *>
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent]((EngineState.selected[F].set(Map.empty) withEvent ClearLoadedSequences(user.some)).toHandle))

    override def setConditions(q: EventQueue[F], conditions: Conditions, user: UserDetails): F[Unit] =
      logDebugEvent(q, "SeqexecEngine: Setting conditions") *>
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent]((EngineState.conditions[F].set(conditions) >>> refreshSequences withEvent SetConditions(conditions, user.some)).toHandle))

    override def setImageQuality(q: EventQueue[F], iq: ImageQuality, user: UserDetails): F[Unit] =
      logDebugEvent(q, "SeqexecEngine: Setting image quality") *>
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](((EngineState.conditions[F] ^|-> Conditions.iq).set(iq) >>> refreshSequences withEvent SetImageQuality(iq, user.some)).toHandle))

    override def setWaterVapor(q: EventQueue[F], wv: WaterVapor, user: UserDetails): F[Unit] =
      logDebugEvent(q, "SeqexecEngine: Setting water vapor") *>
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](((EngineState.conditions[F] ^|-> Conditions.wv).set(wv) >>> refreshSequences withEvent SetWaterVapor(wv, user.some)).toHandle))

    override def setSkyBackground(q: EventQueue[F], sb: SkyBackground, user: UserDetails): F[Unit] =
      logDebugEvent(q, "SeqexecEngine: Setting sky background") *>
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](((EngineState.conditions[F] ^|-> Conditions.sb).set(sb) >>> refreshSequences withEvent SetSkyBackground(sb, user.some)).toHandle))

    override def setCloudCover(q: EventQueue[F], cc: CloudCover, user: UserDetails): F[Unit] =
      logDebugEvent(q, "SeqexecEngine: Setting cloud cover") *>
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](((EngineState.conditions[F] ^|-> Conditions.cc).set(cc) >>> refreshSequences withEvent SetCloudCover(cc, user.some)).toHandle))

    override def setSkipMark(q: EventQueue[F],
                    seqId: Observation.Id,
                    user: UserDetails,
                    stepId: StepId,
                    v: Boolean): F[Unit] =
      q.enqueue1(Event.skip[F, EngineState[F], SeqEvent](seqId, user, stepId, v)).map(_.asRight)

    override def requestRefresh(q: EventQueue[F], clientId: ClientId): F[Unit] =
      q.enqueue1(Event.poll(clientId))

    private def seqQueueRefreshStream: Stream[F, Either[SeqexecFailure, EventType[F]]] = {
      val fd = Duration(settings.odbQueuePollingInterval.toSeconds, TimeUnit.SECONDS)
      Stream.fixedDelay[F](fd).evalMap(_ => Logger[F].debug("Refresh queue from the odb") *> systems.odb.queuedSequences).flatMap { x =>
        Stream.emit(Event.getState[F, EngineState[F], SeqEvent] { st =>
          Stream.eval(odbLoader.refreshSequenceList(x, st)).flatMap(Stream.emits).some
        }.asRight)
      }.handleErrorWith {
        case e: SeqFailure =>
          Stream.emit(SeqexecFailure.OdbSeqError(e).asLeft)
        case e: Exception =>
          Stream.emit(SeqexecFailure.SeqexecException(e).asLeft)
      }
    }

    private val heartbeatPeriod: FiniteDuration = FiniteDuration(10, TimeUnit.SECONDS)

    private def heartbeatStream: Stream[F, EventType[F]] = {
      // If there is no heartbeat in 5 periods throw an error
      val noHeartbeatDetection =
        SeqexecEngine.failIfNoEmitsWithin[F, EventType[F]](5 * heartbeatPeriod)
      Stream.awakeDelay[F](heartbeatPeriod)
        .as(Event.nullEvent: EventType[F])
        .through(noHeartbeatDetection.andThen(_.recoverWith { case _ =>
            Stream.eval[F, EventType[F]](Event.logErrorMsgF("Seqexec engine heartbeat undetected"))
        }))
    }

    override def eventStream(q: EventQueue[F]): Stream[F, SeqexecEvent] =
      stream(q.dequeue.evalMap(e => Logger[F].debug(s"User Event: $e").as(e))
        .mergeHaltBoth(seqQueueRefreshStream.rethrow.mergeHaltL(heartbeatStream)))(EngineState.default[F]).flatMap(x =>
        Stream.eval(logEventResult(x._1) *> notifyODB(x).attempt)).flatMap {
          case Right((ev, qState)) =>
            val sequences = qState.sequences.values.map(viewSequence).toList
            val event = toSeqexecEvent[F](ev, qState)
            Stream.eval(updateMetrics(ev, sequences).as(event))
          case Left(x) =>
            Stream.eval(Logger[F].error(x)("Error notifying the ODB").as(NullEvent))
      }

    override def stream(p: Stream[F, EventType[F]])(s0: EngineState[F])
    : Stream[F, (EventResult[SeqEvent], EngineState[F])] =
      executeEngine.process(iterateQueues)(p)(s0)

    override def stopObserve(q: EventQueue[F], seqId: Observation.Id, graceful: Boolean): F[Unit] =
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](setObsCmd(seqId, StopGracefully))).whenA(graceful) *>
        q.enqueue1(Event.actionStop[F, EngineState[F], SeqEvent](seqId, translator.stopObserve(seqId, graceful)))

    override def abortObserve(q: EventQueue[F], seqId: Observation.Id): F[Unit] = q.enqueue1(
      Event.actionStop[F, EngineState[F], SeqEvent](seqId, translator.abortObserve(seqId))
    )

    override def pauseObserve(q: EventQueue[F], seqId: Observation.Id, graceful: Boolean): F[Unit] =
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](setObsCmd(seqId, PauseGracefully))).whenA(graceful) *>
        q.enqueue1(Event.actionStop[F, EngineState[F], SeqEvent](seqId, translator.pauseObserve(seqId, graceful)))

    override def resumeObserve(q: EventQueue[F], seqId: Observation.Id): F[Unit] =
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](clearObsCmd(seqId))) *>
        q.enqueue1(Event.getState[F, EngineState[F], SeqEvent](translator.resumePaused(seqId)))

    private def queueO(qid: QueueId): Optional[EngineState[F], ExecutionQueue] =
      EngineState.queues[F] ^|-? index(qid)

    private def cmdStateO(qid: QueueId): Optional[EngineState[F], BatchCommandState] =
      queueO(qid) ^|-> ExecutionQueue.cmdState

    private def addSeqs(qid: QueueId, seqIds: List[Observation.Id]): HandleType[F, Unit] =
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
            case (_, BatchExecState.Completed)       => ((EngineState.queues[F] ^|-? index(qid) ^|-> ExecutionQueue.cmdState)
              .set(BatchCommandState.Idle) >>> {(_, ())}).toHandle
            case (BatchCommandState.Run(o, u, c), _) => executeEngine.get.flatMap(st2 => runSequences(shouldSchedule(qid,
              seqs.toSet)(st2), o, u, c))
            case _                                   => executeEngine.unit
          })
      ).getOrElse(executeEngine.unit)}

    override def addSequencesToQueue(q: EventQueue[F], qid: QueueId, seqIds: List[Observation.Id])
    : F[Unit] = q.enqueue1(
      Event.modifyState[F, EngineState[F], SeqEvent](addSeqs(qid, seqIds)
        .as[SeqEvent](UpdateQueueAdd(qid, seqIds)))
    )

    override def addSequenceToQueue(q: EventQueue[F], qid: QueueId, seqId: Observation.Id): F[Unit] =
      addSequencesToQueue(q, qid, List(seqId))

    private def removeSeq(qid: QueueId, seqId: Observation.Id): HandleType[F, Unit] =
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

    override def removeSequenceFromQueue(q: EventQueue[F], qid: QueueId, seqId: Observation.Id)
    : F[Unit] = q.enqueue1(
      Event.modifyState[F, EngineState[F], SeqEvent](
        executeEngine.get.flatMap(st => removeSeq(qid, seqId)
          .as(UpdateQueueRemove(qid, List(seqId), st.queues.get(qid)
            .map(_.queue.indexOf(seqId)).toList))))
    ).map(_.asRight)

    private def moveSeq(qid: QueueId, seqId: Observation.Id, delta: Int): Endo[EngineState[F]] = st =>
      st.queues.get(qid).filter(_.queue.contains(seqId)).map {_ =>
        queueO(qid).modify(_.moveSeq(seqId, delta))(st)
      }.getOrElse(st)

    override def moveSequenceInQueue(q: EventQueue[F], qid: QueueId, seqId: Observation.Id, delta: Int, cid: ClientId)
    : F[Unit] = q.enqueue1(
      Event.modifyState[F, EngineState[F], SeqEvent](
        executeEngine.get.flatMap(_ => (moveSeq(qid, seqId, delta) withEvent UpdateQueueMoved(qid,
          cid, seqId, 0)).toHandle))
      ).map(_.asRight)

    private def clearQ(qid: QueueId): Endo[EngineState[F]] = st =>
      st.queues.get(qid).filter(_.status(st) =!= BatchExecState.Running).map { _ =>
        queueO(qid).modify(_.clear)(st)
      }.getOrElse(st)

    override def clearQueue(q: EventQueue[F], qid: QueueId): F[Unit] = q.enqueue1(
      Event.modifyState[F, EngineState[F], SeqEvent](
        (clearQ(qid) withEvent UpdateQueueClear(qid)).toHandle)
    ).map(_.asRight)

    private def setObserverAndSelect(sid: Observation.Id, observer: Observer, user: UserDetails, clientId: ClientId)
    : HandleType[F, Unit] = Handle(
      StateT[F, EngineState[F], (Unit, Option[Stream[F, EventType[F]]])] {
        st:EngineState[F] => (
          (EngineState.sequences[F] ^|-? index(sid)).getOption(st).map{ obsseq =>
            (EngineState.sequences[F].modify(_ + (sid -> obsseq.copy(observer = observer.some))) >>>
              refreshSequence(sid) >>>
              EngineState.instrumentLoadedL[F](obsseq.seqGen.instrument).set(sid.some) >>>
              {(_, ((), Stream[Pure, EventType[F]](
                Event.modifyState[F, EngineState[F], SeqEvent](
                  { {s:EngineState[F] => s} withEvent
                    AddLoadedSequence(obsseq.seqGen.instrument, sid, user, clientId)
                  }.toHandle
                )
              ).covary[F].some))}
            )(st)
          }.getOrElse((st, ((), None)))
        ).pure[F]
      }
    )

    private def runSequences(ss: Set[Observation.Id], observer: Observer, user: UserDetails, clientId: ClientId)
    :HandleType[F, Unit] =
      ss.map(sid => setObserverAndSelect(sid, observer, user, clientId) *> executeEngine.start(sid, clientId,
        { _ =>true }
      )).fold(Handle.unit[F, EngineState[F], EventType[F]]){_ *> _}

    /**
     * runQueue starts the queue. It founds the top eligible sequences in the queue, and runs them.
     */
    private def runQueue(qid: QueueId, observer: Observer, user: UserDetails, clientId: ClientId)
    : HandleType[F, Unit] = {

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
                                freed: Set[Resource]): HandleType[F, Unit] = {
      executeEngine.get.map(nextRunnableObservations(qid, freed)).flatMap(runSequences(_, observer, user, clientId))
    }

    override def startQueue(q: EventQueue[F], qid: QueueId, observer: Observer, user: UserDetails, clientId: ClientId)
    : F[Unit] = q.enqueue1(
      Event.modifyState[F, EngineState[F], SeqEvent](executeEngine.get.flatMap{ st => {
        queueO(qid).getOption(st).filterNot(_.queue.isEmpty).map {
          _.status(st) match {
            case BatchExecState.Idle |
                 BatchExecState.Stopping => ((EngineState.queues[F] ^|-? index(qid) ^|-> ExecutionQueue.cmdState)
              .set(BatchCommandState.Run(observer, user, clientId)) >>> {(_, ())}).toHandle *>
              runQueue(qid, observer, user, clientId)
            case _                       => executeEngine.unit
          }
        }.getOrElse(executeEngine.unit)
      }}.as(StartQueue(qid, clientId)))
    ).map(_.asRight)

    private def stopSequencesInQueue(qid: QueueId): HandleType[F, Unit] =
      executeEngine.get.map(st =>
        queueO(qid).getOption(st)
          .foldMap(_.queue.filter(sid => EngineState.sequenceStateIndex[F](sid)
            .getOption(st).exists(_.status.isRunning)))
      ).flatMap(_.map(executeEngine.pause).fold(executeEngine.unit)(_ *> _))

    override def stopQueue(q: EventQueue[F], qid: QueueId, clientId: ClientId): F[Unit] = q.enqueue1(
      Event.modifyState[F, EngineState[F], SeqEvent](executeEngine.get.flatMap{ st =>
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

    private val iterateQueues: PartialFunction[SystemEvent[F], HandleType[F, Unit]] = {
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

    private def configSystemCheck(sid: Observation.Id, sys: Resource)(st: EngineState[F]): Boolean = {
      // Resources used by running sequences
      val used = resourcesInUse(st)

      // Resources reserved by running queues, excluding `sid` to prevent self blocking
      val reservedByQueues = resourcesReserved(EngineState.sequences[F].modify(_ - sid)(st))

      !(used ++ reservedByQueues).contains(sys)
    }

    private def configSystemHandle(sid: Observation.Id, stepId: StepId, sys: Resource, clientID: ClientId)
    : HandleType[F, SeqEvent] = {

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
    override def configSystem(q: EventQueue[F], sid: Observation.Id, stepId: StepId, sys: Resource, clientID: ClientId)
    : F[Unit] =
      q.enqueue1(
        Event.modifyState[F, EngineState[F], SeqEvent](configSystemHandle(sid, stepId, sys, clientID))
      ).map(_.asRight)

    def notifyODB(i: (EventResult[SeqEvent], EngineState[F])): F[(EventResult[SeqEvent], EngineState[F])] = {
      (i match {
        case (SystemUpdate(SystemEvent.Failed(id, _, e), _), _) =>
          Logger[F].error(s"Error executing ${id.format} due to $e") *>
          systems.odb
            .obsAbort(id, e.msg)
            .ensure(
              SeqexecFailure
                .Unexpected("Unable to send ObservationAborted message to ODB.")
            )(identity)
        case (SystemUpdate(SystemEvent.Executed(id), _), st) if EngineState.sequenceStateIndex(id).getOption(st)
          .exists(_.status === SequenceState.Idle) =>
          systems.odb.obsPause(id, "Sequence paused by user").void
        case (SystemUpdate(SystemEvent.Finished(id), _), _)     => systems.odb.sequenceEnd(id).void
        case _                                  => Applicative[F].unit
      }).as(i)
    }

    /**
     * Update some metrics based on the event types
     */
    def updateMetrics(e: EventResult[SeqEvent], sequences: List[SequenceView]): F[Unit] = {
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

    private def updateSequenceEndo(seqId: Observation.Id, obsseq: SequenceData[F])
    : Endo[EngineState[F]] = st =>
      executeEngine.update(seqId, toStepList(obsseq.seqGen, HeaderExtraData(st.conditions,
        st.operator, obsseq.observer)))(st)

    private def refreshSequence(id: Observation.Id): Endo[EngineState[F]] = (st:EngineState[F]) => {
      st.sequences.get(id).map(obsseq => updateSequenceEndo(id, obsseq)).foldLeft(st){case (s, f) => f(s)}
    }

    private def refreshSequences: Endo[EngineState[F]] = (st:EngineState[F]) => {
      st.sequences.map{ case (id, obsseq) => updateSequenceEndo(id, obsseq) }.foldLeft(st){case (s, f) => f(s)}
    }

  }

  def createTranslator[F[_]: Sync: Logger](site: Site, systems: Systems[F])
  : F[SeqTranslate[F]] =
    SeqTranslate(site, systems)

  private def splitWhere[A](l: List[A])(p: A => Boolean): (List[A], List[A]) =
    l.splitAt(l.indexWhere(p))

  private def systemsBeingConfigured[F[_]](st: EngineState[F]): Set[Resource] =
    st.sequences.values.filter(d => d.seq.status.isError || d.seq.status.isIdle).toList
      .flatMap(s => s.seq.getSingleActionStates.filter(_._2.started).keys.toList
        .mapFilter(s.seqGen.resourceAtCoords)
      ).toSet

  /**
   * Resource in use = Resources used by running sequences, plus the systems that are being configured because a user
   * commanded a manual configuration apply.
   */
  private def resourcesInUse[F[_]](st: EngineState[F]): Set[Resource] =
    st.sequences.values.toList.mapFilter(s => s.seq.status.isRunning.option(s.seqGen.resources)).foldK ++
      systemsBeingConfigured(st)

  /**
   * Resources reserved by running queues.
   */
  private def resourcesReserved[F[_]](st: EngineState[F]): Set[Resource] = {
    def reserved(q: ExecutionQueue): Set[Resource] = q.queue.fproduct(st.sequences.get).collect{
      case (_, Some(s)) if s.seq.status.isIdle => s.seqGen.resources
    }.foldK

    val runningQs = st.queues.values.filter(_.status(st).running)

    runningQs.map(reserved).toList.foldK

  }

  /**
    * Creates a stream that will follow a heartbeat and raise an error if the heartbeat
    * doesn't get emitted for timeout
    *
    * Credit: Fabio Labella
    * https://gitter.im/functional-streams-for-scala/fs2?at=5e0a6efbfd580457e79aaf0a
    */
  def failIfNoEmitsWithin[F[_]: Concurrent: Timer, A](
      timeout: FiniteDuration
  ): Pipe[F, A, A] = in => {
    import scala.concurrent.TimeoutException
    def now = Timer[F].clock.monotonic(NANOSECONDS).map(_.nanos)

    Stream.eval(now.flatMap(Ref[F].of)).flatMap { lastActivityAt =>
      in.evalTap(_ => now.flatMap(lastActivityAt.set))
        .concurrently {
          Stream.repeatEval {
            (now, lastActivityAt.get)
              .mapN(_ - _)
              .flatMap { elapsed =>
                val t = timeout - elapsed

                Sync[F]
                  .raiseError[Unit](new TimeoutException)
                  .whenA(t <= 0.nanos) >> Timer[F].sleep(t)
              }
          }
        }
    }
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
  def findRunnableObservations[F[_]](qid: QueueId)(st: EngineState[F])
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
  private def nextRunnableObservations[F[_]](qid: QueueId, freed: Set[Resource])(st: EngineState[F])
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
  private def shouldSchedule[F[_]](qid: QueueId, sids: Set[Observation.Id])(st: EngineState[F]): Set[Observation.Id] =
    findRunnableObservations(qid)(st).intersect(sids)

  /**
   * Build the seqexec and setup epics
   */
  def build[F[_]: ConcurrentEffect: Timer: Logger](
    site: Site,
    systems: Systems[F],
    conf: SeqexecEngineConfiguration,
    metrics: SeqexecMetrics)(
    implicit executeEngine: ExecEngineType[F]
  ): F[SeqexecEngine[F]] =
    createTranslator(site, systems)
      .map{ new SeqexecEngineImpl[F](systems, conf, metrics, _) }

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

  private def executionQueueViews[F[_]](st: EngineState[F]): SortedMap[QueueId, ExecutionQueueView] = {
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
        ), obsSeq.pendingObsCmd)
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

  def logEventResult[F[_]: Logger: Applicative](ev: EventResult[SeqEvent]): F[Unit] =
    ev match {
      case UserCommandResponse(ue, _, _) =>
        Logger[F].debug(s"User response event: ${ue.getClass.getName}")
      case SystemUpdate(se, _)             =>
        Logger[F].debug(s"System event event: ${se.getClass.getName}")
    }

  def logEvent[F[_]: Logger: Applicative](prefix: String)(v: SeqexecEvent): Stream[F, SeqexecEvent] =
    Stream.eval(Logger[F].debug(s"$prefix: ${v.getClass.getName}") *> v.pure[F])

  private def toSeqexecEvent[F[_]](ev: EventResult[SeqEvent], qState: EngineState[F]): SeqexecEvent = {
    val sequences = qState.sequences.values.map(viewSequence).toList
    // Building the view is a relatively expensive operation
    // By putting it into a def we only incur that cost if the message requires it
    def svs =
      SequencesQueue(
      EngineState.selected[F].get(qState),
      EngineState.conditions[F].get(qState),
      EngineState.operator[F].get(qState),
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
        case UserEvent.LogDebug(_, _)         => NullEvent
        case UserEvent.LogInfo(m, ts)         => ServerLogMessage(ServerLogLevel.INFO, ts, m)
        case UserEvent.LogWarning(m, ts)      => ServerLogMessage(ServerLogLevel.WARN, ts, m)
        case UserEvent.LogError(m, ts)        => ServerLogMessage(ServerLogLevel.ERROR, ts, m)
        case UserEvent.ActionResume(_, _, _)  => SequenceUpdated(svs)
      }
      case SystemUpdate(se, _)             => se match {
        // TODO: Sequence completed event not emitted by engine.
        case SystemEvent.Completed(_, _, _, _)                                    => SequenceUpdated(svs)
        case SystemEvent.StopCompleted(id, _, _, _)                               => SequenceStopped(id, svs)
        case SystemEvent.Aborted(id, _, _, _)                                     => SequenceAborted(id, svs)
        case SystemEvent.PartialResult(_, _, _, Partial(_: InternalPartialVal))   => NullEvent
        case SystemEvent.PartialResult(i, s, _, Partial(ObsProgress(t, r, v)))    =>
          ObservationProgressEvent(ObservationProgress(i, s, t, r.self, v))
        case SystemEvent.PartialResult(i, s, _, Partial(NSProgress(t, r, v, u)))  =>
          ObservationProgressEvent(NSObservationProgress(i, s, t, r.self, v, u))
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
          singleActionEvent[F, SingleActionOp.Completed](c, qState, SingleActionOp.Completed)
        case SystemEvent.SingleRunFailed(c, r)   =>
          singleActionEvent[F, SingleActionOp.Error](c, qState, SingleActionOp.Error.apply(_, _, _, r.msg))
      }
    }
  }

  private def singleActionEvent[F[_], S <: SingleActionOp](c: ActionCoords,
                                                     qState: EngineState[F],
                                                     f: (Observation.Id, StepId, Resource) => S): SeqexecEvent =
    qState.sequences.get(c.sid).flatMap(_.seqGen.resourceAtCoords(c.actCoords))
      .map(res => SingleActionEvent(f(c.sid, c.actCoords.stepId, res)))
      .getOrElse(NullEvent)

}
