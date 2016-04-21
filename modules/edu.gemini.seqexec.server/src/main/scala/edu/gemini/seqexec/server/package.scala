package edu.gemini.seqexec

import edu.gemini.seqexec.server.SeqexecFailure.SeqexecException

import scala.language.higherKinds
import scalaz._
import Scalaz._

import scalaz.concurrent.Task

package object server {

  type TrySeq[A] = SeqexecFailure \/ A

  object TrySeq {
    def apply[A](a: A): TrySeq[A]             = a.right[SeqexecFailure]
    def fail[A](p: SeqexecFailure): TrySeq[A] = p.left[A]
  }

  type SeqAction[A] = EitherT[Task, SeqexecFailure, A]

  type SeqObserve[A, B] = Reader[A, SeqAction[B]]

  object SeqAction {
    def apply[A](a: => A): SeqAction[A]          = SeqAction(a)
    def fail[A](p: SeqexecFailure): SeqAction[A] = EitherT(Task.delay(TrySeq.fail(p)))
  }

  implicit class SeqActionOps[A](a: SeqAction[A]) {
    def runSeqAction: TrySeq[A] = a.run.unsafePerformSyncAttempt.leftMap[SeqexecFailure](SeqexecException).join
  }

  implicit class MoreDisjunctionOps[A,B](ab: A \/ B) {
    def validationNel: ValidationNel[A, B] =
      ab match {
        case -\/(a) => Failure(NonEmptyList(a))
        case \/-(b) => Success(b)
      }
  }
}

