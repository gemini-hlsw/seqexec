// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec

import edu.gemini.seqexec.engine.Event

import scalaz._
import Scalaz._
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

  type EventQueue = Queue[Event]

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
}
