package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * A list of Steps grouped by target and instrument.
  */
case class Sequence[A](id: String, steps: List[Step[A]]) {

  def isEmpty: Boolean = steps.isEmpty

  def cons(step: Step[A]): Sequence[A] = Sequence(id, step :: steps)

  def uncons: Option[(Step[A], Sequence[A])] =
    steps.headOption.map((_, Sequence(id, steps.tail)))

}

object Sequence {

  def empty[A](id: String): Sequence[A] = Sequence(id, Nil)

  def steps[A]: Sequence[A] @> List[Step[A]] =
    Lens.lensu((s, sts) => s.copy(steps = sts), _.steps)

  implicit val SequenceFunctor = new Functor[Sequence] {
    def map[A, B](fa: Sequence[A])(f: A => B): Sequence[B] =
      Sequence(fa.id, fa.steps.map(_.map(f)))
  }

}


case class SequenceZ(
  pending: Sequence[Action],
  focus: StepZ,
  done: Sequence[Result]
) {

  val next: Option[SequenceZ] =
    focus.next match {
      // Step completed
      case None => for {
        stepDone <- focus.uncurrentify
        (stepPending, seq) <- pending.uncons
        curr <- StepZ.currentify(stepPending)
      } yield SequenceZ(seq, curr, done.cons(stepDone))
      // Current step ongoing
      case Some(stz) => Some(SequenceZ(pending, stz, done))
    }

  val uncurrentify: Option[Sequence[Result]] =
    if (pending.isEmpty) focus.uncurrentify.map(done.cons)
    else None
}

object SequenceZ {

  def currentify(seq0: Sequence[Action]): Option[SequenceZ] = for {
    (step, seq1) <- seq0.uncons
    curr <- StepZ.currentify(step)
  } yield SequenceZ(seq1, curr, Sequence.empty(seq0.id))

  private val focus: SequenceZ @> StepZ =
    Lens.lensu((s, f) => s.copy(focus = f), _.focus)

  val current: SequenceZ @> Current = focus >=> StepZ.current

}
