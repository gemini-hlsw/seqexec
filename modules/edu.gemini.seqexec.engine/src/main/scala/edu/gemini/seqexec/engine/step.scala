package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * A list of `Executions` grouped by observation.
  */
case class Step[+A](id: Int, executions: List[Execution[A]])

object Step {

  def status(step: Step[Action \/ Result]): Status =
    if (step.executions.isEmpty || step.all(_.isLeft)) Status.Waiting
    else if (step.all(_.isRight)) Status.Completed
    else Status.Running

  implicit val stepFunctor = new Functor[Step] {
    def map[A, B](fa: Step[A])(f: A => B): Step[B] =
      Step(fa.id, fa.executions.map(_.map(f)))
  }

  // TODO: Proof Foldable laws
  implicit val stepFoldable = new Foldable[Step] {
    def foldMap[A, B](fa: Step[A])(f: A => B)(implicit F: scalaz.Monoid[B]): B =
      // TODO: Foldable composition?
      fa.executions.foldMap(_.foldMap(f))

    def foldRight[A, B](fa: Step[A], z: => B)(f: (A, => B) => B): B =
      fa.executions.foldRight(z)((l, b) => l.foldRight(b)(f(_, _)))

  }

}

case class StepZ(
  id: Int,
  pending: List[Execution[Action]],
  focus: Current,
  done: List[Execution[Result]]
) {

  val next: Option[StepZ] =
    pending match {
      case exep :: exeps => for {
        // TODO: Applicative syntax?
        curr <- Current.currentify(exep)
        exed <- focus.uncurrentify
      } yield StepZ(id, exeps, curr, exed :: done)
      case Nil => None
    }

  val uncurrentify: Option[Step[Result]] =
    if (pending.isEmpty) focus.uncurrentify.map(x => Step(id, x :: done))
    else None

  val toStep: Step[Action \/ Result] =
    Step(
      id,
      // TODO: Functor composition?
      done.map(_.map(_.right)) ++
      List(focus.execution) ++
      pending.map(_.map(_.left))
    )

}

object StepZ {

  def currentify(step: Step[Action]): Option[StepZ] =
    step.executions match {
      case exe :: exes =>
        Current.currentify(exe).map(
          StepZ(step.id, exes, _, Nil)
        )
      case Nil => None
    }

  val current: StepZ @> Current =
    Lens.lensu((s, f) => s.copy(focus = f), _.focus)

}
