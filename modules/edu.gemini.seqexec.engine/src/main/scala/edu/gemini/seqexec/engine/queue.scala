package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * A list of Sequences. The `Queue` could be empty of Sequences when waiting
  * for the addition of new ones.
  */
case class Queue[A](sequences: List[Sequence[A]]) {

  def isEmpty: Boolean = sequences.isEmpty

  def cons(seq: Sequence[A]): Queue[A] = Queue(seq :: sequences)

  def uncons: Option[(Sequence[A], Queue[A])] =
    sequences.headOption.map((_, Queue(sequences.tail)))

}

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
  pending: Queue[Action],
  focus: SequenceZ,
  done: Queue[Result]
) {

  val next: Option[QueueZ] =
    focus.next match {
      // Sequence completed
      case None =>
        for {
          seqDone <- focus.uncurrentify
          (seqPending, q) <- pending.uncons
          curr <- SequenceZ.currentify(seqPending)
        } yield QueueZ(q, curr, done.cons(seqDone))
      // Current Sequence ongoing
      case Some(seqz) => Some(QueueZ(pending, seqz, done))
    }

  val uncurrentify: Option[Queue[Result]] =
    if (pending.isEmpty) focus.uncurrentify.map(done.cons)
    else None

}

object QueueZ {

  def currentify(queue0: Queue[Action]): Option[QueueZ] = for {
      (seq, queue1) <- queue0.uncons
      curr <- SequenceZ.currentify(seq)
    } yield QueueZ(queue1, curr, Queue(Nil))

  private val focus: QueueZ @> SequenceZ =
    Lens.lensu((q, f) => q.copy(focus = f), _.focus)

  val current: QueueZ @> Current = focus >=> SequenceZ.current

}

