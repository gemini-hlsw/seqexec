// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.syntax.all._
import monocle.Lens
import monocle.macros.GenLens
import monocle.macros.Lenses
import seqexec.engine.Action.ActionState
import seqexec.model.StepId
import seqexec.model.StepState

/**
 * A list of `Executions` grouped by observation.
 */
@Lenses
final case class Step[F[_]](
  id:         StepId,
  breakpoint: Step.BreakpointMark,
  skipped:    Step.Skipped,
  skipMark:   Step.SkipMark,
  executions: List[ParallelActions[F]]
)

object Step {

  @Lenses
  final case class BreakpointMark(self: Boolean) extends AnyVal

  @Lenses
  final case class SkipMark(self: Boolean) extends AnyVal

  @Lenses
  final case class Skipped(self: Boolean) extends AnyVal

  def breakpointL[F[_]]: Lens[Step[F], Boolean] = Step.breakpoint.andThen(BreakpointMark.self)
  def skippedL[F[_]]: Lens[Step[F], Boolean]    = Step.skipped.andThen(Skipped.self)

  def init[F[_]](id: StepId, executions: List[ParallelActions[F]]): Step[F] =
    Step(id = id,
         breakpoint = BreakpointMark(false),
         skipped = Skipped(false),
         skipMark = SkipMark(false),
         executions = executions
    )

  /**
   * Calculate the `Step` `Status` based on the underlying `Action`s.
   */
  private def status[F[_]](step: Step[F]): StepState =
    if (step.skipped.self) StepState.Skipped
    else
      // Find an error in the Step
      step.executions
        .flatMap(_.toList)
        .find(Action.errored)
        .flatMap { x =>
          x.state.runState match {
            case ActionState.Failed(Result.Error(msg)) => msg.some
            case _                                     => None
            // Return error or continue with the rest of the checks
          }
        }
        .map[StepState](StepState.Failed)
        .getOrElse(
          // All actions in this Step were completed successfully, or the Step is empty.
          if (step.executions.flatMap(_.toList).exists(Action.aborted)) StepState.Aborted
          else if (step.executions.flatMap(_.toList).forall(Action.completed)) StepState.Completed
          else if (step.executions.flatMap(_.toList).forall(_.state.runState.isIdle))
            StepState.Pending
          // Not all actions are completed or pending.
          else StepState.Running
        )

  implicit class StepOps[F[_]](val s: Step[F]) extends AnyVal {
    def status: StepState = Step.status(s)
  }

  /**
   * Step Zipper. This structure is optimized for the actual `Step` execution.
   */
  final case class Zipper[F[_]](
    id:         Int,
    breakpoint: BreakpointMark,
    skipMark:   SkipMark,
    pending:    List[ParallelActions[F]],
    focus:      Execution[F],
    done:       List[ParallelActions[F]],
    rolledback: (Execution[F], List[ParallelActions[F]])
  ) { self =>

    /**
     * Adds the `Current` `Execution` to the list of completed `Execution`s and makes the next
     * pending `Execution` the `Current` one.
     *
     * If there are still `Action`s that have not finished in `Current` or if there are no more
     * pending `Execution`s it returns `None`.
     */
    val next: Option[Zipper[F]] =
      pending match {
        case Nil           => None
        case exep :: exeps =>
          (Execution.currentify(exep), focus.uncurrentify).mapN((curr, exed) =>
            self.copy(pending = exeps, focus = curr, done = exed.prepend(done))
          )
      }

    def rollback: Zipper[F] =
      self.copy(pending = rolledback._2, focus = rolledback._1, done = Nil)

    /**
     * Obtain the resulting `Step` only if all `Execution`s have been completed. This is a special
     * way of *unzipping* a `Zipper`.
     */
    val uncurrentify: Option[Step[F]] =
      if (pending.isEmpty)
        focus.uncurrentify.map(x => Step(id, breakpoint, Skipped(false), skipMark, x.prepend(done)))
      else None

    /**
     * Unzip a `Zipper`. This creates a single `Step` with either completed `Exection`s or pending
     * `Execution`s.
     */
    val toStep: Step[F] =
      Step(
        id = id,
        breakpoint = breakpoint,
        skipped = Skipped(false),
        skipMark = skipMark,
        executions = done ++ focus.toParallelActionsList ++ pending
      )

    val skip: Step[F] = toStep.copy(skipped = Skipped(true))

    def update(executions: List[ParallelActions[F]]): Zipper[F] =
      Zipper
        .calcRolledback(executions)
        .map { case r @ (_, exes) =>
          // Changing `pending` allows to propagate changes to non executed `executions`, even if the step is running
          // Don't do it if the number of executions changes. In that case the update will only have an effect if
          // the step is (re)started.
          if (exes.length === done.length + pending.length)
            this.copy(pending = exes.takeRight(pending.length), rolledback = r)
          else this.copy(rolledback = r)
        }
        .getOrElse(this)

  }

  object Zipper {

    private def calcRolledback[F[_]](
      executions: List[ParallelActions[F]]
    ): Option[(Execution[F], List[ParallelActions[F]])] = executions match {
      case Nil         => None
      case exe :: exes =>
        Execution.currentify(exe).map((_, exes))
    }

    /**
     * Make a `Zipper` from a `Step` only if all the `Execution`s in the `Step` are pending. This is
     * a special way of *zipping* a `Step`.
     */
    def currentify[F[_]](step: Step[F]): Option[Zipper[F]] =
      calcRolledback(step.executions).map { case (x, exes) =>
        Zipper(
          step.id,
          step.breakpoint,
          step.skipMark,
          exes,
          x,
          Nil,
          (x, exes)
        )
      }

    def current[F[_]]: Lens[Zipper[F], Execution[F]] =
      GenLens[Zipper[F]](_.focus)

  }

}
