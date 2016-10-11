package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * A list of Steps grouped by target and instrument.
  */
case class Sequence[+A](id: String, steps: List[Step[A]])

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
  id: String,
  pending: List[Step[Action]],
  focus: StepZ,
  done: List[Step[Result]]
) {

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

  val uncurrentify: Option[Sequence[Result]] =
    if (pending.isEmpty) focus.uncurrentify.map(x => Sequence(id, x :: done))
    else None

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
