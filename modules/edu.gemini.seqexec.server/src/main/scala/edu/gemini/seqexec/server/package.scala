// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec

import edu.gemini.seqexec.engine.{ActionMetadata, ActionMetadataGenerator, Engine, Sequence}
import edu.gemini.seqexec.model.Model.{CloudCover, Conditions, ImageQuality, Observer, Operator, SequenceState, SkyBackground, WaterVapor}
import edu.gemini.seqexec.model.UserDetails
import scalaz.{-\/, EitherT, Failure, NonEmptyList, Reader, Success, ValidationNel, \/, \/-}
import scalaz.syntax.either._
import scalaz.concurrent.Task
import scalaz.stream.async.mutable.Queue
import monocle.Lens
import monocle.macros.GenLens


package server {
  final case class EngineMetadata(queues: ExecutionQueues, conditions: Conditions, operator: Option[Operator])
  object EngineMetadata {
    val default: EngineMetadata = EngineMetadata(Map(CalibrationQueueName -> Nil), Conditions.default, None)

    val queuesL: Lens[EngineMetadata, ExecutionQueues] = GenLens[EngineMetadata](_.queues)

    val conditionsL: Lens[EngineMetadata, Conditions] = GenLens[EngineMetadata](_.conditions)

    val operatorL: Lens[EngineMetadata, Option[Operator]] = GenLens[EngineMetadata](_.operator)
  }

  sealed trait SeqEvent
  final case class SetOperator(name: Operator, user: Option[UserDetails]) extends SeqEvent
  final case class SetObserver(id: Sequence.Id, user: Option[UserDetails], name: Observer) extends SeqEvent
  final case class SetConditions(conditions: Conditions, user: Option[UserDetails]) extends SeqEvent
  final case class SetImageQuality(iq: ImageQuality, user: Option[UserDetails]) extends SeqEvent
  final case class SetWaterVapor(wv: WaterVapor, user: Option[UserDetails]) extends SeqEvent
  final case class SetSkyBackground(wv: SkyBackground, user: Option[UserDetails]) extends SeqEvent
  final case class SetCloudCover(cc: CloudCover, user: Option[UserDetails]) extends SeqEvent
  case object NullSeqEvent extends SeqEvent
}

package object server {

  val CalibrationQueueName: String = "Calibration Queue"

  type TrySeq[A] = SeqexecFailure \/ A

  object TrySeq {
    def apply[A](a: A): TrySeq[A]             = a.right[SeqexecFailure]
    def fail[A](p: SeqexecFailure): TrySeq[A] = p.left[A]
  }

  type SeqAction[A] = EitherT[Task, SeqexecFailure, A]

  type SeqObserve[A, B] = Reader[A, SeqAction[B]]

  type ExecutionQueue = List[Sequence.Id]
  type ExecutionQueues = Map[String, ExecutionQueue]

  implicit object ExecutionQueuesCanGenerateActionMetadata extends ActionMetadataGenerator[EngineMetadata] {
    override def generate(a: EngineMetadata)(v: ActionMetadata): ActionMetadata =
      v.copy(conditions = a.conditions, operator = a.operator)
  }

  val executeEngine: Engine[EngineMetadata, SeqEvent] = new Engine[EngineMetadata, SeqEvent]

  type EventQueue = Queue[executeEngine.EventType]

  object SeqAction {
    def apply[A](a: => A): SeqAction[A]          = EitherT(Task.delay(TrySeq(a)))
    def either[A](a: => TrySeq[A]): SeqAction[A] = EitherT(Task.delay(a))
    def fail[A](p: SeqexecFailure): SeqAction[A] = EitherT(Task.delay(TrySeq.fail(p)))
    def void: SeqAction[Unit]                    = SeqAction.apply(())
  }

  implicit class MoreDisjunctionOps[A,B](ab: A \/ B) {
    def validationNel: ValidationNel[A, B] =
      ab match {
        case -\/(a) => Failure(NonEmptyList(a))
        case \/-(b) => Success(b)
      }
  }

  implicit class ExecutionQueueOps(q: ExecutionQueue) {
    def status(st: executeEngine.StateType): SequenceState = {
      val statuses: List[SequenceState] = q.map(st.sequences.get(_).map(_.status).getOrElse(SequenceState.Idle))

      statuses.find(_.isRunning).orElse(statuses.find(_.isError)).orElse(statuses.find(_.isStopped))
        .orElse(statuses.find(_.isIdle)).getOrElse(SequenceState.Completed)
    }
  }

}
