package edu.gemini.seqexec

import edu.gemini.seqexec.server.SeqexecFailure.{SeqexecException, Unexpected}
import edu.gemini.seqexec.server.System

import scalaz._
import Scalaz._

import scalaz.concurrent.Task

/**
 * Created by jluhrs on 5/18/15.
 */
package object server {

  type TrySeq[A] = SeqexecFailure \/ A

  object TrySeq {
    def apply[A](a: A): TrySeq[A]             = a.right[SeqexecFailure]
    def fail[A](p: SeqexecFailure): TrySeq[A] = p.left[A]
  }

  //type SeqAction[+A] = EitherT[Task, SeqexecFailure, A]
  type SeqAction[+A] = Task[SeqexecFailure \/ A]

  object SeqAction {
    def apply[A](a: => A): SeqAction[A]          = Task(a.right)
    def fail[A](p: SeqexecFailure): SeqAction[A] = Task(p.left)
  }

  implicit class SeqActionOps[A](a: SeqAction[A]) {
    def runSeqAction: TrySeq[A] = a.attemptRun.leftMap[SeqexecFailure](SeqexecException).join
  }

  implicit class MoreDisjunctionOps[A,B](ab: A \/ B) {
    def validationNel: ValidationNel[A, B] =
      ab match {
        case -\/(a) => Failure(NonEmptyList(a))
        case \/-(b) => Success(b)
      }
  }

    // This is built into scalaz 7.1
  implicit class MoreMonadOps[M[+_], A](ma: M[A])(implicit M: Monad[M]) {
    def whileM_[A](p: M[Boolean]): M[Unit] =
      M.ifM(p, M.bind(ma)(_ => whileM_(p)), M.point(()))
  }

}

