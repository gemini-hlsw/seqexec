// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec

import cats.{Applicative, ApplicativeError, Endo, Eq, Functor, MonadError}
import cats.data._
import cats.effect.IO
import cats.implicits._
import edu.gemini.spModel.`type`.SequenceableSpType
import edu.gemini.spModel.guide.StandardGuideOptions
import fs2.concurrent.Queue
import fs2.Stream
import gem.Observation
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.chrisdavenport.log4cats.Logger
import monocle.macros.Lenses
import monocle.Lens
import monocle.Optional
import monocle.macros.GenLens
import monocle.function.Index._
import monocle.function.At._
import seqexec.engine.Engine
import seqexec.engine.Result.PauseContext
import seqexec.engine.Result
import seqexec.model._
import seqexec.model.enum._
import seqexec.engine._
import seqexec.server.SequenceGen.StepGen
import squants.Time

package server {
  @Lenses
  final case class EngineState[F[_]](
    queues: ExecutionQueues,
    selected: Map[Instrument, Observation.Id],
    conditions: Conditions,
    operator: Option[Operator],
    sequences: Map[Observation.Id, SequenceData[F]]
  )

  object EngineState {
    def default[F[_]]: EngineState[F] =
      EngineState[F](
        Map(CalibrationQueueId -> ExecutionQueue.init(CalibrationQueueName)),
        Map.empty,
        Conditions.Default,
        None,
        Map.empty
      )

    def instrumentLoadedL[F[_]](
      instrument: Instrument
    ): Lens[EngineState[F], Option[Observation.Id]] =
      GenLens[EngineState[F]](_.selected) ^|-> at(instrument)

    def atSequence[F[_]](sid:Observation.Id): Optional[EngineState[F], SequenceData[F]] =
      EngineState.sequences ^|-? index(sid)

    def sequenceStateIndex[F[_]](sid: Observation.Id): Optional[EngineState[F], Sequence.State[F]] =
      atSequence[F](sid) ^|-> SequenceData.seq

    def engineState[F[_]]: Engine.State[F, EngineState[F]] = new Engine.State[F, EngineState[F]] {
      override def sequenceStateIndex(
                                       sid: Observation.Id
                                     ): Optional[EngineState[F], Sequence.State[F]] =
        EngineState.sequenceStateIndex(sid)
    }

    implicit final class WithEventOps[F[_]](val f: Endo[EngineState[F]]) extends AnyVal {
      def withEvent(ev: SeqEvent): EngineState[F] => (EngineState[F], SeqEvent) = f >>> {(_, ev)}
    }
  }

  final case class HeaderExtraData(conditions: Conditions, operator: Option[Operator], observer: Option[Observer])
  object HeaderExtraData {
    val default: HeaderExtraData = HeaderExtraData(Conditions.Default, None, None)
  }

  final case class ObserveContext[F[_]](
    resumePaused: Time => Stream[F, Result[F]],
    stopPaused: Stream[F, Result[F]],
    abortPaused: Stream[F, Result[F]],
    expTime: Time
  ) extends PauseContext[F]

}

package object server {
  implicit def geEq[D <: SequenceableSpType]: Eq[D] =
    Eq[String].contramap(_.sequenceValue())

  implicit val sgoEq: Eq[StandardGuideOptions.Value] =
    Eq[Int].contramap(_.ordinal())

  type TrySeq[A]                 = Either[SeqexecFailure, A]

  object TrySeq {
    def apply[A](a: A): TrySeq[A]              = Either.right(a)
    def fail[A](p:  SeqexecFailure): TrySeq[A] = Either.left(p)
  }

  type ExecutionQueues = Map[QueueId, ExecutionQueue]

  // This is far from ideal but we'll address this in another refactoring
  private implicit def logger: Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("seqexec-engine")

  // TODO move this out of being a global. This act as an anchor to the rest of the code
  implicit val executeEngine: Engine[IO, EngineState[IO], SeqEvent] =
    new Engine[IO, EngineState[IO], SeqEvent](EngineState.engineState[IO])

  type EventQueue[F[_]] = Queue[F, EventType[F]]

  implicit class EitherTFailureOps[F[_]: MonadError[?[_], Throwable], A](s: EitherT[F, SeqexecFailure, A]) {
    def liftF: F[A] =
      s.value.flatMap(_.liftTo[F])
  }

  implicit class EitherTOps[F[_],  A, B](fa: EitherT[F, A, B]) {
    def widenRethrowT[T](
      implicit me: MonadError[F, T],
               at: A <:< T
    ): F[B] =
      fa.leftMap(at).rethrowT
  }

  // This assumes that there is only one instance of e in l
  private def moveElement[T](l: List[T], e: T, delta: Int)(implicit eq: Eq[T]): List[T] = {
    val idx = l.indexOf(e)

    if (delta === 0 || idx < 0) {
      l
    } else {
      val (h, t) = l.filterNot(_ === e).splitAt(idx + delta)
      (h :+ e) ++ t
    }
  }

  implicit class ExecutionQueueOps[F[_]](val q: ExecutionQueue) extends AnyVal {
    def status(st: EngineState[F]): BatchExecState = {
      val statuses: Seq[SequenceState] = q.queue.map(sid => st.sequences.get(sid))
        .collect{ case Some(x) => x }
        .map(_.seq.status)

      q.cmdState match {
        case BatchCommandState.Idle         => BatchExecState.Idle
        case BatchCommandState.Run(_, _, _) => if(statuses.forall(_.isCompleted)) BatchExecState.Completed
                                               else if(statuses.exists(_.isRunning)) BatchExecState.Running
                                                    else BatchExecState.Waiting
        case BatchCommandState.Stop         => if(statuses.exists(_.isRunning)) BatchExecState.Stopping
                                               else BatchExecState.Idle
      }
    }

    def addSeq(sid: Observation.Id): ExecutionQueue = q.copy(queue = q.queue :+ sid)
    def addSeqs(sids: List[Observation.Id]): ExecutionQueue = q.copy(queue = q.queue ++ sids)
    def removeSeq(sid: Observation.Id): ExecutionQueue = q.copy(queue = q.queue.filter(_ =!= sid))
    def moveSeq(sid:Observation.Id, delta: Int): ExecutionQueue = q.copy(queue = moveElement(q.queue, sid, delta))
    def clear: ExecutionQueue = q.copy(queue = List.empty)
  }

  implicit final class ToHandle[F[_]: Applicative, A](f: EngineState[F] => (EngineState[F], A)) {
    import Handle.StateToHandle
    def toHandle: HandleType[F, A] =
      StateT[F, EngineState[F], A]{ st => f(st).pure[F] }.toHandle
  }

  def toStepList[F[_]](seq: SequenceGen[F], d: HeaderExtraData): List[engine.Step[F]] =
    seq.steps.map(StepGen.generate(_, d))

  // If f is true continue, otherwise fail
  def failUnlessM[F[_]: MonadError[?[_], Throwable]](f: F[Boolean], err: Exception): F[Unit] =
    f.flatMap {
      MonadError[F, Throwable].raiseError(err).unlessA
    }

  implicit class ResponseToResult(val r: Either[Throwable, Response]) extends AnyVal {
    def toResult[F[_]]: Result[F] = r.fold(e => e match {
      case e: SeqexecFailure => Result.Error(SeqexecFailure.explain(e))
      case e: Throwable      => Result.Error(SeqexecFailure.explain(SeqexecFailure.SeqexecException(e)))
    }, r => Result.OK(r))
  }

  implicit class RecoverResultErrorOps[F[_]: ApplicativeError[?[_], Throwable]](r: F[Result[F]]) {
    def safeResult: F[Result[F]] = r.recover {
      case e: SeqexecFailure => Result.Error(SeqexecFailure.explain(e))
      case e: Throwable      => Result.Error(SeqexecFailure.explain(SeqexecFailure.SeqexecException(e)))
    }
  }

  def catchObsErrors[F[_]](t: Throwable)(implicit L: Logger[F]): Stream[F, Result[F]] = t match {
    case e: SeqexecFailure =>
      Stream.eval(L.error(e)(s"Observation error: ${SeqexecFailure.explain(e)}")) *>
      Stream.emit(Result.Error(SeqexecFailure.explain(e)))
    case e: Throwable =>
      Stream.eval(L.error(e)(s"Observation error: ${e.getMessage}")) *>
      Stream.emit(Result.Error(SeqexecFailure.explain(SeqexecFailure.SeqexecException(e))))
  }

  implicit class ActionResponseToAction[F[_]: Functor: ApplicativeError[?[_], Throwable], A <: Response](val x: F[A]) {
    def toAction(kind: ActionType): Action[F] = fromF[F](kind, x.attempt.map(_.toResult))
  }

  implicit class ConfigResultToAction[F[_]: Functor](val x: F[ConfigResult[F]]) {
    def toAction(kind: ActionType): Action[F] = fromF[F](kind, x.map(r => Result.OK(Response.Configured(r.sys.resource))))
  }

  // Some types defined to avoid repeating long type definitions everywhere
  type EventType[F[_]] = Event[F, EngineState[F], SeqEvent]
  type HandleType[F[_], A] = Handle[F, EngineState[F], EventType[F], A]
  type ExecEngineType[F[_]] = Engine[F, EngineState[F], SeqEvent]

}
