// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec

import edu.gemini.seqexec.engine.{Engine, Event, Sequence}
import edu.gemini.seqexec.model.Model.SequenceState

import scalaz.{-\/, EitherT, Failure, NonEmptyList, Reader, Success, ValidationNel, \/, \/-}
import scalaz.syntax.either._
import scalaz.concurrent.Task
import scalaz.stream.async.mutable.Queue

package object server {

  type TrySeq[A] = SeqexecFailure \/ A

  object TrySeq {
    def apply[A](a: A): TrySeq[A]             = a.right[SeqexecFailure]
    def fail[A](p: SeqexecFailure): TrySeq[A] = p.left[A]
  }

  type SeqAction[A] = EitherT[Task, SeqexecFailure, A]

  type SeqObserve[A, B] = Reader[A, SeqAction[B]]

  type ExecutionQueue = List[Sequence.Id]
  type ExecutionQueues = Map[String, ExecutionQueue]
  type EngineEvent = Event[ExecutionQueues]
  type EngineState = Engine.State[ExecutionQueues]

  val executeEngine: Engine[ExecutionQueues] = new Engine[ExecutionQueues]

  type EventQueue = Queue[EngineEvent]

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
    def status(st: EngineState): SequenceState = {
      val statuses: List[SequenceState] = q.map(st.sequences.get(_).map(_.status).getOrElse(SequenceState.Idle))

      statuses.find(_.isRunning).orElse(statuses.find(_.isError)).orElse(statuses.find(_.isStopped))
        .orElse(statuses.find(_.isIdle)).getOrElse(SequenceState.Completed)
    }
  }

}
