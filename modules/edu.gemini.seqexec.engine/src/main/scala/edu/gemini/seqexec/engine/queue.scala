package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * A list of Sequences. The `Queue` could be empty of Sequences when waiting
  * for the addition of new ones.
  */
case class Queue[+A](sequences: List[Sequence[A]])

object Queue {

  def sequences[A]: Queue[A] @> List[Sequence[A]] =
    Lens.lensu((q, s) => q.copy(sequences = s), _.sequences)

  implicit def queueMonoid[A]: Monoid[Queue[A]] = new Monoid[Queue[A]] {
    def append(a: Queue[A], b: => Queue[A]): Queue[A] =
      Queue(a.sequences ::: b.sequences)

    val zero: Queue[A] = Queue(Nil)
  }

  implicit val queueFunctor = new Functor[Queue] {
    def map[A, B](fa: Queue[A])(f: A => B): Queue[B] =
      Queue(fa.sequences.map(_.map(f)))
  }

}

case class QueueZ(
  pending: List[Sequence[Action]],
  focus: SequenceZ,
  done: List[Sequence[Result]]
) {

  val next: Option[QueueZ] =
    focus.next match {
      // Sequence completed
      case None =>
        pending match {
          case seqp :: seqps => for {
            curr <- SequenceZ.currentify(seqp)
            seqd <- focus.uncurrentify
          } yield QueueZ(seqps, curr, seqd :: done)
          case Nil => None
        }
      // Current Sequence ongoing
      case Some(seqz) => Some(QueueZ(pending, seqz, done))
    }

  val uncurrentify: Option[Queue[Result]] =
    if (pending.isEmpty) focus.uncurrentify.map(x => Queue(x :: done))
    else None

  val toQueue: Queue[Action \/ Result] =
    Queue(
      done.map(_.map(_.right)) ++
      List(focus.toSequence) ++
      pending.map(_.map(_.left))
    )
}

object QueueZ {

  def currentify(queue: Queue[Action]): Option[QueueZ] =
    queue.sequences match {
      case seq :: seqs =>
        SequenceZ.currentify(seq).map(
          QueueZ(seqs, _, Nil)
        )
      case Nil => None
    }

  private val focus: QueueZ @> SequenceZ =
    Lens.lensu((q, f) => q.copy(focus = f), _.focus)

  val current: QueueZ @> Current = focus >=> SequenceZ.current

}
