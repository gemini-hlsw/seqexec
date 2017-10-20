// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.engine

import edu.gemini.seqexec.model.Model.{Resource, StepConfig, StepState}

import scalaz.{Monoid, Functor, Foldable, \/, \/-}
import scalaz.syntax.apply._
import scalaz.syntax.foldable._
import scalaz.syntax.either._
import scalaz.std.AllInstances._

import monocle.Lens
import monocle.macros.GenLens

/**
  * A list of `Executions` grouped by observation.
  */
final case class Step[+A](
  id: Int,
  fileId: Option[FileId],
  config: StepConfig,
  resources: Set[Resource],
  breakpoint: Boolean,
  skip: Boolean,
  executions: List[List[A]]
)

object Step {

  type Id = Int

  implicit val stepFunctor: Functor[Step] = new Functor[Step] {
    def map[A, B](step: Step[A])(f: A => B): Step[B] =
      step.copy(executions = step.executions.map(_.map(f)))
  }

  // TODO: Proof Foldable laws
  implicit val stepFoldable: Foldable[Step] = new Foldable[Step] {
    def foldMap[A, B](fa: Step[A])(f: A => B)(implicit F: Monoid[B]): B =
      // TODO: Foldable composition?
      fa.executions.foldMap(_.foldMap(f))

    def foldRight[A, B](fa: Step[A], z: => B)(f: (A, => B) => B): B =
      fa.executions.foldRight(z)((l, b) => l.foldRight(b)(f(_, _)))
  }

  /**
    * Calculate the `Step` `Status` based on the underlying `Action`s.
    */
  def status(step: Step[Action \/ Result]): StepState = {
    def stepCompleted(s: Action \/ Result): Boolean = s match {
      case \/-(Result.OK(_)) => true
      case _                 => false
    }

    // Find an error in the Step
    step.findLeft(Execution.errored).flatMap(
      // Get the message if there is one
      _.fold(_ => None, _.errMsg)
      // Return error or continue with the rest of the checks
    ).map(StepState.Error).getOrElse(
      // It's possible to have a Step with empty executions when a completed
      // Step is loaded from the ODB.
      if (step.executions.isEmpty || step.executions.all(_.isEmpty)) StepState.Completed
      // All actions in this Step are pending.
      else if (step.all(_.isLeft)) StepState.Pending
      // All actions in this Step were completed successfully.
      else if (step.all(stepCompleted)) StepState.Completed
      // Not all actions are completed or pending.
      else StepState.Running
    )

  }

  /**
    * Step Zipper. This structure is optimized for the actual `Step` execution.
    *
    */
  final case class Zipper(
    id: Int,
    fileId: Option[FileId],
    config: StepConfig,
    resources: Set[Resource],
    breakpoint: Boolean,
    skip: Boolean,
    pending: List[Actions],
    focus: Execution,
    done: List[Results],
    rolledback: (Execution, List[Actions])
  ) { self =>

    /**
      * Adds the `Current` `Execution` to the list of completed `Execution`s and
      * makes the next pending `Execution` the `Current` one.
      *
      * If there are still `Action`s that have not finished in `Current` or if
      * there are no more pending `Execution`s it returns `None`.
      */
    val next: Option[Zipper] =
      pending match {
        case Nil           => None
        case exep :: exeps =>
          (Execution.currentify(exep) |@| focus.uncurrentify) (
            (curr, exed) => self.copy(pending = exeps, focus = curr, done = exed :: done)
          )
      }

    def rollback: Zipper =
      self.copy(pending = rolledback._2, focus = rolledback._1, done = Nil)

    /**
      * Obtain the resulting `Step` only if all `Execution`s have been completed.
      * This is a special way of *unzipping* a `Zipper`.
      *
      */
    val uncurrentify: Option[Step[Result]] =
      if (pending.isEmpty) focus.uncurrentify.map(
        x => Step(id, fileId, config, resources, breakpoint, skip, x :: done)
      )
      else None

    /**
      * Unzip a `Zipper`. This creates a single `Step` with either completed
      * `Exection`s or pending `Execution`s.
      */
    val toStep: Step[Action \/ Result] =
      Step(
        id,
        fileId,
        config,
        resources,
        breakpoint,
        skip,
        // TODO: Functor composition?
        done.map(_.map(_.right)) ++
          List(focus.execution) ++
          pending.map(_.map(_.left)
          )
      )
  }

  object Zipper {

    /**
      * Make a `Zipper` from a `Step` only if all the `Execution`s in the `Step` are
      * pending. This is a special way of *zipping* a `Step`.
      *
      */
    def currentify(step: Step[Action]): Option[Zipper] =
      step.executions match {
        case Nil         => None
        case exe :: exes =>
          Execution.currentify(exe).map(x =>
            Zipper(
              step.id,
              step.fileId,
              step.config,
              step.resources,
              step.breakpoint,
              step.skip,
              exes,
              x,
              Nil,
              (x, exes)
            )
          )
      }

    val current: Lens[Zipper, Execution] =
      GenLens[Zipper](_.focus)

    val fileId: Lens[Zipper, Option[FileId]] =
      GenLens[Zipper](_.fileId)

  }

}
