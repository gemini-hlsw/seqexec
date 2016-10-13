package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * A list of `Sequence`s.
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

/**
  * Queue Zipper. This structure is optimized for the actual `Queue`
  * execution.
  *
  */
case class QueueZ(
  pending: List[Sequence[Action]],
  focus: SequenceZ,
  done: List[Sequence[Result]]
) {

  /**
    * Runs the next execution. If the current `Sequence` is completed it adds
    * the `SequenceZ` under focus to the list of completed `Sequence`s and makes
    * the next pending `Sequence` the current one.
    *
    * If there are still `Step`s that have not finished in the current
    * `Sequence` or if there are no more pending `Seqeunce`s it returns `None`.
    */
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

  /**
    * Obtain the `Queue` results only if all `Step`s have been completed.
    * This is a special way of *unzipping* a `SequenceZ`.
    *
    */
  val uncurrentify: Option[Queue[Result]] =
    if (pending.isEmpty) focus.uncurrentify.map(x => Queue(x :: done))
    else None

  /**
    * Unzip `QueueZ`. This creates a single `Sequence` with either completed
    * `Step`s or pending `Step`s.
    */
  val toQueue: Queue[Action \/ Result] =
    Queue(
      done.map(_.map(_.right)) ++
      List(focus.toSequence) ++
      pending.map(_.map(_.left))
    )
}

object QueueZ {

  /**
    * Make a `QueueZ` from a `Queue` only if all the `Sequence`s in the
    * `Queue` are pending. This is a special way of *zipping* a `Queue`.
    *
    */
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
