package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * A list of `Step`s grouped by target and instrument.
  */
case class Sequence[+A](id: String, steps: List[Step[A]])

object Sequence {

  def empty[A](id: String): Sequence[A] = Sequence(id, Nil)

  /**
    * Calculate the `Sequence` `Status` based on the underlying `Action`s.
    *
    */
  def status(seq: Sequence[Action \/ Result]): Status =
    if (seq.steps.isEmpty || seq.all(_.isLeft)) Status.Waiting
    else if (seq.all(_.isRight)) Status.Completed
    else Status.Running

  implicit val SequenceFunctor = new Functor[Sequence] {
    def map[A, B](fa: Sequence[A])(f: A => B): Sequence[B] =
      Sequence(fa.id, fa.steps.map(_.map(f)))
  }

  // TODO: Proof Foldable laws
  implicit val stepFoldable = new Foldable[Sequence] {
    def foldMap[A, B](fa: Sequence[A])(f: A => B)(implicit F: scalaz.Monoid[B]): B =
      // TODO: Foldable composition?
      fa.steps.foldMap(_.foldMap(f))

    def foldRight[A, B](fa: Sequence[A], z: => B)(f: (A, => B) => B): B =
      fa.steps.foldRight(z)((l, b) => l.foldRight(b)(f(_, _)))

  }

}

/**
  * Sequence Zipper. This structure is optimized for the actual `Sequence`
  * execution.
  *
  */
case class SequenceZ(
  id: String,
  pending: List[Step[Action]],
  focus: StepZ,
  done: List[Step[Result]]
) {

  /**
    * Runs the next execution. If the current `Step` is completed it adds the
    * `StepZ` under focus to the list of completed `Step`s and makes the next
    * pending `Step` the current one.
    *
    * If there are still `Execution`s that have not finished in the current
    * `Step` or if there are no more pending `Step`s it returns `None`.
    */
  val next: Option[SequenceZ] =
    focus.next match {
      // Step completed
      case None =>
        pending match {
          case stepp :: stepps => for {
            // TODO: Applicative style?
            curr  <- StepZ.currentify(stepp)
            stepd <- focus.uncurrentify
          } yield SequenceZ(id, stepps, curr, stepd :: done)
          case Nil => None
        }
      // Current step ongoing
      case Some(stz) => Some(SequenceZ(id, pending, stz, done))
    }

  /**
    * Obtain the resulting `Sequence` only if all `Step`s have been completed.
    * This is a special way of *unzipping* a `SequenceZ`.
    *
    */
  val uncurrentify: Option[Sequence[Result]] =
    if (pending.isEmpty) focus.uncurrentify.map(x => Sequence(id, x :: done))
    else None

  /**
    * Unzip a `SequenceZ`. This creates a single `Sequence` with either
    * completed `Step`s or pending `Step`s.
    */
  val toSequence: Sequence[Action \/ Result] =
    Sequence(
      id,
      // TODO: Functor composition?
      done.map(_.map(_.right)) ++
      List(focus.toStep) ++
      pending.map(_.map(_.left))
    )
}

object SequenceZ {

  /**
    * Make a `SequenceZ` from a `Sequence` only if all the `Step`s in the
    * `Sequence` are pending. This is a special way of *zipping* a `Sequence`.
    *
    */
  def currentify(seq: Sequence[Action]): Option[SequenceZ] =
    seq.steps match {
      case step :: steps =>
        StepZ.currentify(step).map(
          SequenceZ(seq.id, steps, _, Nil)
        )
      case Nil => None
    }

  private val focus: SequenceZ @> StepZ =
    Lens.lensu((s, f) => s.copy(focus = f), _.focus)

  val current: SequenceZ @> Current = focus >=> StepZ.current

}
