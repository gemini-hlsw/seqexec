package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * A list of `Executions` grouped by observation.
  */
case class Step[A](id: Int, executions: List[Execution[A]]) {

  def isEmpty: Boolean = executions.isEmpty

  /**
    * Adds a `Execution` to the front of a `Step`.
    */
  def cons(exe: Execution[A]): Step[A] = Step(id, exe :: executions)

  /**
    * Return the next `Execution` and the remaining `Step` if there are more
    * `Execution`s left.
    */
  def uncons: Option[(Execution[A], Step[A])] =
    executions.headOption.map((_, Step(id, executions.tail)))

}

object Step {

  def empty[A](id: Int): Step[A] = Step(id, Nil)

  def executions[A]: Step[A] @> List[Execution[A]] =
    Lens.lensu((s, exes) => s.copy(executions = exes), _.executions)

  implicit val stepFunctor = new Functor[Step] {
    def map[A, B](fa: Step[A])(f: A => B): Step[B] =
      Step(fa.id, fa.executions.map(_.map(f)))
  }

}

case class StepZ(
  pending: Step[Action],
  focus: Current,
  done: Step[Result]
) {

  val next: Option[StepZ] = for {
    exeDone <- focus.uncurrentify
    (exePending, step) <- pending.uncons
    curr <- Current.currentify(exePending)
  } yield StepZ(step, curr, done.cons(exeDone))


  val uncurrentify: Option[Step[Result]] =
    if (pending.isEmpty) focus.uncurrentify.map(done.cons)
    else None

}

object StepZ {

  def currentify(step0: Step[Action]): Option[StepZ] = for {
    (exe, step1) <- step0.uncons
    curr <- Current.currentify(exe)
  } yield StepZ(step1, curr, Step.empty(step0.id))

  val current: StepZ @> Current =
    Lens.lensu((s, f) => s.copy(focus = f), _.focus)
}
