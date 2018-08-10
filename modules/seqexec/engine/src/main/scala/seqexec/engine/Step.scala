// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import seqexec.model.enum.Resource
import seqexec.model.{ StepConfig, StepState }

import cats.implicits._
import monocle.Lens
import monocle.macros.GenLens

/**
  * A list of `Executions` grouped by observation.
  */
final case class Step(
  id: Step.Id,
  fileId: Option[FileId],
  config: StepConfig,
  resources: Set[Resource],
  breakpoint: Step.BreakpointMark,
  skipped: Step.Skipped,
  skipMark: Step.SkipMark,
  executions: List[List[Action]]
)

object Step {

  type Id = Int

  final case class BreakpointMark(self: Boolean) extends AnyVal
  final case class SkipMark(self: Boolean) extends AnyVal
  final case class Skipped(self: Boolean) extends AnyVal

  def init(id: Id,
           fileId: Option[FileId],
           config: StepConfig,
           resources: Set[Resource],
           executions: List[List[Action]]): Step = Step(id, fileId, config, resources, BreakpointMark(false), Skipped(false), SkipMark(false), executions)

  /**
    * Calculate the `Step` `Status` based on the underlying `Action`s.
    */
  def status(step: Step): StepState = {

    if(step.skipped.self) StepState.Skipped
    else
      // Find an error in the Step
      step.executions.flatten.find(Action.errored).flatMap { x => x.state.runState match {
        case Action.Failed(Result.Error(msg)) => msg.some
        case _                                => None
        // Return error or continue with the rest of the checks
      }}.map(StepState.Failed).getOrElse(
        // All actions in this Step were completed successfully, or the Step is empty.
        if (step.executions.flatten.forall(Action.completed)) StepState.Completed
        else if (step.executions.flatten.forall(_.state.runState.isIdle)) StepState.Pending
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
    breakpoint: BreakpointMark,
    skipMark: SkipMark,
    pending: List[Actions],
    focus: Execution,
    done: List[Actions],
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
          (Execution.currentify(exep), focus.uncurrentify).mapN (
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
    val uncurrentify: Option[Step] =
      if (pending.isEmpty) focus.uncurrentify.map(
        x => Step(id, fileId, config, resources, breakpoint, Skipped(false), skipMark, x :: done)
      )
      else None

    /**
      * Unzip a `Zipper`. This creates a single `Step` with either completed
      * `Exection`s or pending `Execution`s.
      */
    val toStep: Step =
      Step(
        id,
        fileId,
        config,
        resources,
        breakpoint,
        Skipped(false),
        skipMark,
        done ++ List(focus.execution) ++ pending
      )

    val skip: Step = toStep.copy(skipped = Skipped(true))

  }

  object Zipper {

    /**
      * Make a `Zipper` from a `Step` only if all the `Execution`s in the `Step` are
      * pending. This is a special way of *zipping* a `Step`.
      *
      */
    def currentify(step: Step): Option[Zipper] =
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
              step.skipMark,
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
