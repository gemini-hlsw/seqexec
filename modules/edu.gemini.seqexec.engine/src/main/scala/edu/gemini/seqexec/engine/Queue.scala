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

  /**
    * Queue Zipper. This structure is optimized for the actual `Queue`
    * execution.
    *
    */
  case class Zipper(
    pending: List[Sequence[Action]],
    focus: Sequence.Zipper,
    done: List[Sequence[Result]]
  ) {

    /**
      * Runs the next execution. If the current `Sequence` is completed it adds
      * the `Sequence.Zipper` under focus to the list of completed `Sequence`s and makes
      * the next pending `Sequence` the current one.
      *
      * If there are still `Step`s that have not finished in the current
      * `Sequence` or if there are no more pending `Seqeunce`s it returns `None`.
      */
    val next: Option[Zipper] =
      focus.next match {
        // Sequence completed
        case None       =>
          pending match {
            case Nil           => None
            case seqp :: seqps => for {
              curr <- Sequence.Zipper.currentify(seqp)
              seqd <- focus.uncurrentify
            } yield Zipper(seqps, curr, seqd :: done)
          }
        // Current Sequence ongoing
        case Some(seqz) => Some(Zipper(pending, seqz, done))
      }

    /**
      * Obtain the `Queue` results only if all `Step`s have been completed.
      * This is a special way of *unzipping* a `Sequence.Zipper`.
      *
      */
    val uncurrentify: Option[Queue[Result]] =
      if (pending.isEmpty) focus.uncurrentify.map(x => Queue(x :: done))
      else None

    /**
      * Unzip `Zipper`. This creates a single `Sequence` with either completed
      * `Step`s or pending `Step`s.
      */
    val toQueue: Queue[Action \/ Result] =
      Queue(
        done.map(_.map(_.right)) ++
        List(focus.toSequence) ++
        pending.map(_.map(_.left))
      )

  }

  object Zipper {

    /**
      * Make a `Zipper` from a `Queue` only if all the `Sequence`s in the
      * `Queue` are pending. This is a special way of *zipping* a `Queue`.
      *
      */
    def currentify(queue: Queue[Action]): Option[Zipper] =
      queue.sequences match {
        case Nil         => None
        case seq :: seqs =>
          Sequence.Zipper.currentify(seq).map(
            Zipper(seqs, _, Nil)
          )
      }

    private val focus: Zipper @> Sequence.Zipper =
      Lens.lensu((q, f) => q.copy(focus = f), _.focus)

    val current: Zipper @> Execution = focus >=> Sequence.Zipper.current

  }

}
