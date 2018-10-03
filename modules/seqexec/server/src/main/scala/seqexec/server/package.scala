// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec

import cats.data._
import cats.effect.IO
import cats.effect.Sync
import cats.implicits._
import cats.Applicative
import cats.ApplicativeError
import cats.Eq
import cats.Functor
import edu.gemini.spModel.`type`.SequenceableSpType
import edu.gemini.spModel.guide.StandardGuideOptions
import fs2.async.mutable.Queue
import gem.Observation
import monocle.macros.Lenses
import monocle.Lens
import monocle.macros.GenLens
import monocle.function.At.at
import monocle.function.At.atMap
import seqexec.engine.Engine
import seqexec.model.ClientID
import seqexec.model.CalibrationQueueId
import seqexec.model.CalibrationQueueName
import seqexec.model.ExecutionQueue
import seqexec.model.QueueId
import seqexec.model.Conditions
import seqexec.model.Observer
import seqexec.model.Operator
import seqexec.model.SequenceState
import seqexec.model.enum._
import seqexec.model.Notification
import seqexec.model.UserDetails

package server {

  @Lenses
  final case class ObserverSequence(observer: Option[Observer], seq: SequenceGen)
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object ObserverSequence

  @Lenses
  final case class EngineState(queues: ExecutionQueues, selected: Map[Instrument, Observation.Id], conditions: Conditions, operator: Option[Operator], sequences: Map[Observation.Id, ObserverSequence], executionState: Engine.State)
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object EngineState {
    val default: EngineState = EngineState(Map(CalibrationQueueId -> ExecutionQueue.init(CalibrationQueueName)), Map.empty, Conditions.Default, None, Map.empty, Engine.State.empty)

    def instrumentLoadedL(instrument: Instrument): Lens[EngineState, Option[Observation.Id]] = GenLens[EngineState](_.selected) ^|-> at(instrument)

  }

  sealed trait SeqEvent
  final case class SetOperator(name: Operator, user: Option[UserDetails]) extends SeqEvent
  final case class SetObserver(id: Observation.Id, user: Option[UserDetails], name: Observer) extends SeqEvent
  final case class SetConditions(conditions: Conditions, user: Option[UserDetails]) extends SeqEvent
  final case class LoadSequence(sid: Observation.Id) extends SeqEvent
  final case class UnloadSequence(id: Observation.Id) extends SeqEvent
  final case class AddLoadedSequence(instrument: Instrument, sid: Observation.Id, user: UserDetails, clientId: ClientID) extends SeqEvent
  final case class ClearLoadedSequences(user: Option[UserDetails]) extends SeqEvent
  final case class SetImageQuality(iq: ImageQuality, user: Option[UserDetails]) extends SeqEvent
  final case class SetWaterVapor(wv: WaterVapor, user: Option[UserDetails]) extends SeqEvent
  final case class SetSkyBackground(wv: SkyBackground, user: Option[UserDetails]) extends SeqEvent
  final case class SetCloudCover(cc: CloudCover, user: Option[UserDetails]) extends SeqEvent
  final case class NotifyUser(memo: Notification, clientID: ClientID) extends SeqEvent
  final case class StartQueue(qid: QueueId, clientID: ClientID) extends SeqEvent
  final case class StopQueue(qid: QueueId, clientID: ClientID) extends SeqEvent
  final case class UpdateQueue(qid: QueueId) extends SeqEvent
  case object NullSeqEvent extends SeqEvent

  sealed trait ControlStrategy
  // System will be fully controlled by Seqexec
  case object FullControl extends ControlStrategy
  // Seqexec connects to system, but only to read values
  case object ReadOnly extends ControlStrategy
  // All system interactions are internally simulated
  case object Simulated extends ControlStrategy

  object ControlStrategy {
    def fromString(v: String): Option[ControlStrategy] = v match {
      case "full"      => Some(FullControl)
      case "readOnly"  => Some(ReadOnly)
      case "simulated" => Some(Simulated)
      case _           => None
    }
  }

  final case class HeaderExtraData(conditions: Conditions, operator: Option[Operator], observer: Option[Observer])
  object HeaderExtraData {
    val default: HeaderExtraData = HeaderExtraData(Conditions.Default, None, None)
  }

}

package object server {
  implicit def geEq[D <: SequenceableSpType]: Eq[D] =
    Eq[String].contramap(_.sequenceValue())

  implicit val sgoEq: Eq[StandardGuideOptions.Value] =
    Eq[Int].contramap(_.ordinal())

  type TrySeq[A] = Either[SeqexecFailure, A]
  type ApplicativeErrorSeq[F[_]] = ApplicativeError[F, SeqexecFailure]

  object TrySeq {
    def apply[A](a: A): TrySeq[A]             = Either.right(a)
    def fail[A](p: SeqexecFailure): TrySeq[A] = Either.left(p)
  }

  type SeqAction[A] = EitherT[IO, SeqexecFailure, A]
  type SeqActionF[F[_], A] = EitherT[F, SeqexecFailure, A]

  type SeqObserve[A, B] = Reader[A, SeqAction[B]]
  type SeqObserveF[F[_], A, B] = Reader[A, SeqActionF[F, B]]

  type ExecutionQueues = Map[QueueId, ExecutionQueue]

  val executeEngine: Engine[EngineState, SeqEvent] = new Engine[EngineState, SeqEvent](EngineState.executionState)

  type EventQueue = Queue[IO, executeEngine.EventType]

  object SeqAction {
    def apply[A](a: => A): SeqAction[A]          = EitherT(IO.apply(TrySeq(a)))
    def either[A](a: => TrySeq[A]): SeqAction[A] = EitherT(IO.apply(a))
    def fail[A](p: SeqexecFailure): SeqAction[A] = EitherT(IO.apply(TrySeq.fail(p)))
    def void: SeqAction[Unit]                    = SeqAction.apply(())
  }

  object SeqActionF {
    def apply[F[_]: Sync, A](a: => A): SeqActionF[F, A]       = EitherT(Sync[F].delay(TrySeq(a)))
    def liftF[F[_]: Functor, A](a: => F[A]): SeqActionF[F, A] = EitherT.liftF(a)
    def void[F[_]: Applicative]: SeqActionF[F, Unit]          = EitherT.liftF(Applicative[F].pure(()))
  }

  implicit class MoreDisjunctionOps[A,B](ab: Either[A, B]) {
    def validationNel: ValidatedNel[A, B] =
      ab.fold(a => Validated.Invalid(NonEmptyList.of(a)), b => Validated.Valid(b))
  }

  // This assumes that there is only one instance of e in l
  private def moveElement[T](l: List[T], e: T, d: Int)(implicit eq: Eq[T]): List[T] = {
    val idx = l.indexOf(e)

    if(d === 0 || idx<0) l
    else {
      val (h, t) = l.filterNot(_ === e).splitAt(idx+d)
      (h :+ e) ++ t
    }
  }

  implicit class ExecutionQueueOps(val q: ExecutionQueue) extends AnyVal {
    def status(st: EngineState): BatchExecState = {
      val statuses: Seq[SequenceState] = q.queue.map(st.executionState.sequences.get(_).map(_.status))
        .collect{ case Some(x) => x }

      if(statuses.forall(_.isCompleted)) BatchExecState.Completed
      else q.cmdState match {
        case BatchCommandState.Idle   => BatchExecState.Idle
        case BatchCommandState.Run(_) => if(statuses.exists(_.isRunning)) BatchExecState.Running
                                  else BatchExecState.Waiting
        case BatchCommandState.Stop   => if(statuses.exists(_.isRunning)) BatchExecState.Stopping
                                  else BatchExecState.Idle
      }
    }

    def addSeq(sid: Observation.Id): ExecutionQueue = q.copy(queue = q.queue :+ sid)
    def addSeqs(sids: List[Observation.Id]): ExecutionQueue = q.copy(queue = q.queue ++ sids)
    def removeSeq(sid: Observation.Id): ExecutionQueue = q.copy(queue = q.queue.filter(_ =!= sid))
    def moveSeq(sid:Observation.Id, idx: Int): ExecutionQueue = q.copy(queue = moveElement(q.queue, sid, idx))
    def clear: ExecutionQueue = q.copy(queue = List.empty)
  }

  implicit class ControlStrategyOps(v: ControlStrategy) {
    val connect: Boolean = v match {
      case Simulated => false
      case _         => true
    }
    // If connected, then use real values for keywords
    val realKeywords: Boolean = connect
    val command: Boolean = v match {
      case FullControl => true
      case _           => false
    }
  }

}
