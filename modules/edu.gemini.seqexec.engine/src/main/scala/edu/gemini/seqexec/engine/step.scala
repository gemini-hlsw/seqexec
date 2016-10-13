package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * A list of `Executions` grouped by observation.
  */
case class Step[+A](id: Int, executions: List[Execution[A]])

object Step {

  /**
    * Calculate the `Step` `Status` based on the underlying `Action`s.
    *
    */
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

/**
  * Step Zipper. This structure is optimized for the actual `Step` execution.
  *
  */
case class StepZ(
  id: Int,
  pending: List[Execution[Action]],
  focus: Current,
  done: List[Execution[Result]]
) {

  /**
    * Adds the `Current` `Execution` to the list of completed `Execution`s and
    * makes the next pending `Execution` the `Current` one.
    *
    * If there are still `Action`s that have not finished in `Current` or if
    * there are no more pending `Execution`s it returns `None`.
    */
  val next: Option[StepZ] =
    pending match {
      case exep :: exeps => for {
        // TODO: Applicative syntax?
        curr <- Current.currentify(exep)
        exed <- focus.uncurrentify
      } yield StepZ(id, exeps, curr, exed :: done)
      case Nil => None
    }

  /**
    * Obtain the resulting `Step` only if all `Execution`s have been completed.
    * This is a special way of *unzipping* a `StepZ`.
    *
    */
  val uncurrentify: Option[Step[Result]] =
    if (pending.isEmpty) focus.uncurrentify.map(x => Step(id, x :: done))
    else None

  /**
    * Unzip a `StepZ`. This creates a single `Step` with either completed
    * `Exection`s or pending `Execution`s.
    */
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

  /**
    * Make a `StepZ` from a `Step` only if all the `Execution`s in the `Step` are
    * pending. This is a special way of *zipping* a `Step`.
    *
    */
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
