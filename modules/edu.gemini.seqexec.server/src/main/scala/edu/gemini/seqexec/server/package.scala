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

  type SeqAction[+A] = EitherT[Task, SeqexecFailure, A]

  object SeqAction {
    def apply[A](a: => A): SeqAction[A]          = EitherT(Task(a.right))
    def fail[A](p: SeqexecFailure): SeqAction[A] = EitherT(Task(p.left))
  }

  implicit class SeqActionOps[A](a: SeqAction[A]) {
    def runSeqAction: TrySeq[A] = a.run.attemptRun.leftMap[SeqexecFailure](SeqexecException).join
  }

  val NondeterminismSeq: Nondeterminism[SeqAction] =
    new Nondeterminism[SeqAction] {
      def point[A](a: => A): SeqAction[A] = Monad[SeqAction].point(a)
      def bind[A, B](fa: SeqAction[A])(f: A => SeqAction[B]): SeqAction[B] = Monad[SeqAction].bind(fa)(f)
      def chooseAny[A](head: SeqAction[A], tail: Seq[SeqAction[A]]): SeqAction[(A, Seq[SeqAction[A]])] = {
        EitherT(Nondeterminism[Task].chooseAny(head.run, tail.map(_.run)).map {
          case (-\/(p), _) => -\/(p)
          case (\/-(a), s) => \/-((a, s.map(EitherT(_))))
        })
      }
    }

}

