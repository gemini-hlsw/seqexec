// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.syntax.all._
import monocle.Lens
import monocle.macros.GenLens
import seqexec.engine.Action.ActionState
import seqexec.engine.Result.RetVal
import seqexec.model.Observation
import seqexec.model.SequenceState
import seqexec.model.StepId

/**
  * A list of `Step`s grouped by target and instrument.
  */
final case class Sequence[F[_]](
  id: Observation.Id,
  steps: List[Step[F]]
)

object Sequence {

  def empty[F[_]](id: Observation.Id): Sequence[F] = Sequence(id, Nil)

  /**
    * Sequence Zipper. This structure is optimized for the actual `Sequence`
    * execution.
    *
    */
  final case class Zipper[F[_]](
    id: Observation.Id,
    pending: List[Step[F]],
    focus: Step.Zipper[F],
    done: List[Step[F]]
  ) {

    private val (toSkip, remaining): (List[Step[F]], List[Step[F]]) = pending.span(st => st.skipMark.self)

    /**
      * Runs the next execution. If the current `Step` is completed it adds the
      * `StepZ` under focus to the list of completed `Step`s and makes the next
      * pending `Step` the current one.
      *
      * If there are still `Execution`s that have not finished in the current
      * `Step` or if there are no more pending `Step`s it returns `None`.
      *
      * It skips steps, but honoring breakpoints.
      */
    val next: Option[Zipper[F]] =
      focus.next match {
        // Step completed
        case None      =>
          val (toSkip, remaining): (List[Step[F]], List[Step[F]]) = pending.span(st => st.skipMark.self && !st.breakpoint.self)
          remaining match {
            case Nil => None
            case stepp :: stepps => (Step.Zipper.currentify(stepp), focus.uncurrentify).mapN (
              (curr, stepd) => Zipper(id, stepps, curr, (done :+ stepd) ::: toSkip.map(_.copy(skipped = Step.Skipped(true))))
            )
          }
        // Current step ongoing
        case Some(stz) => Some(Zipper(id, pending, stz, done))
      }

    def rollback: Zipper[F] = this.copy(focus = focus.rollback)

    //Skips steps before starting a sequence.
    def skips: Option[Zipper[F]] = {
      if (focus.skipMark.self) {
        remaining match {
          case Nil => None
          case stepp :: stepps =>
            (Step.Zipper.currentify(stepp), focus.skip.some).mapN (
              (curr, stepd) => Zipper(id, stepps, curr, (done :+ stepd) ::: toSkip.map(_.copy(skipped = Step.Skipped(true))))
            )
        }
      }
      else this.some
    }

    /**
      * Obtain the resulting `Sequence` only if all `Step`s have been completed.
      * This is a special way of *unzipping* a `Zipper`.
      *
      */
    val uncurrentify: Option[Sequence[F]] =
      if (remaining.isEmpty)
        if(focus.skipMark.self) Sequence(id, (done :+ focus.skip) ::: toSkip.map(_.copy(skipped = Step.Skipped(true)))).some
        else focus.uncurrentify.map(x => Sequence(id, (done :+ x) ::: toSkip.map(_.copy(skipped = Step.Skipped(true)))))
      else None

    /**
      * Unzip a `Zipper`. This creates a single `Sequence` with either
      * completed `Step`s or pending `Step`s.
      */
    val toSequence: Sequence[F] =
      Sequence(
        id,
        // TODO: Functor composition?
        done ++ List(focus.toStep) ++ pending
      )
  }

  object Zipper {

    /**
      * Make a `Zipper` from a `Sequence` only if all the `Step`s in the
      * `Sequence` are pending. This is a special way of *zipping* a `Sequence`.
      *
      */
    def currentify[F[_]](seq: Sequence[F]): Option[Zipper[F]] =
      seq.steps match {
        case Nil           => None
        case step :: steps =>
          Step.Zipper.currentify(step).map(
            Zipper(seq.id, steps, _, Nil)
          )
      }

    def zipper[F[_]](seq: Sequence[F]): Option[Zipper[F]] =
      separate(seq).flatMap {
        case (pending, done)   => pending match {
          case Nil             => None
          case s :: ss =>
            Step.Zipper.currentify(s).map(
              Zipper(seq.id, ss, _, done)
            )
        }
      }

    // We would use MonadPlus' `separate` if we wanted to separate Actions or
    // Results, but here we want only Steps.
    private def separate[F[_]](seq: Sequence[F]): Option[(List[Step[F]], List[Step[F]])] = {

      seq.steps.foldLeftM[Option, (List[Step[F]], List[Step[F]])]((Nil, Nil))(
        (acc, step) =>
        if (step.status.isPending)
          acc.leftMap(_ :+ step).some
        else if (step.status.isFinished)
          acc.map(_ :+ step).some
        else none
      )

    }

    def focus[F[_]]: Lens[Zipper[F], Step.Zipper[F]] =
      GenLens[Zipper[F]](_.focus)

    def current[F[_]]: Lens[Zipper[F], Execution[F]] =
      focus ^|-> Step.Zipper.current

  }

  sealed trait State[F[_]] {

    /**
      * Returns a new `State` where the next pending `Step` is been made the
      * current `Step` under execution and the previous current `Step` is
      * placed in the completed `Sequence`.
      *
      * If the current `Step` has `Execution`s not completed or there are no more
      * pending `Step`s it returns `None`.
      */
    val next: Option[State[F]]

    val status: SequenceState

    val pending: List[Step[F]]

    def rollback: State[F]

    def skips: Option[State[F]]

    def setBreakpoint(stepId: StepId, v: Boolean): State[F]

    def setSkipMark(stepId: StepId, v: Boolean): State[F]

    def getCurrentBreakpoint: Boolean

    /**
      * Current Execution
      */
    val current: Execution[F]

    val currentStep: Option[Step[F]]

    val done: List[Step[F]]

    /**
      * Given an index of a current `Action` it replaces such `Action` with the
      * `Result` and returns the new modified `State`.
      *
      * If the index doesn't exist, the new `State` is returned unmodified.
      */
    def mark(i: Int)(r: Result[F]): State[F]

    def start(i: Int): State[F]

    /**
      * Updates the steps executions.
      * It preserves the number of steps.
      * @param stepDefs New executions.
      * @return Updated state
      */
    def update(stepDefs: List[List[ParallelActions[F]]]): State[F]

    /**
      * Unzip `State`. This creates a single `Sequence` with either completed `Step`s
      * or pending `Step`s.
      */
    val toSequence: Sequence[F]

    // Functions to handle single run of Actions
    def startSingle(c: ActionCoordsInSeq): State[F]

    def failSingle(c: ActionCoordsInSeq, err: Result.Error): State[F]

    def completeSingle[V <: RetVal](c: ActionCoordsInSeq, r: V): State[F]

    def getSingleState(c: ActionCoordsInSeq): ActionState[F]

    def getSingleAction(c: ActionCoordsInSeq): Option[Action[F]]

    val getSingleActionStates: Map[ActionCoordsInSeq, ActionState[F]]

    def clearSingles: State[F]

  }

  object State {

    def status[F[_]]: Lens[State[F], SequenceState] =
    // `State` doesn't provide `.copy`
      Lens[State[F], SequenceState](_.status)(s => {
        case Zipper(st, _, x) => Zipper(st, s, x)
        case Final(st, _)  => Final(st, s)
      })

    def isRunning[F[_]](st: State[F]): Boolean = st.status.isRunning

    def userStopRequested[F[_]](st: State[F]): Boolean = st.status.userStopRequested

    def anyStopRequested[F[_]](st: State[F]): Boolean = st.status match {
      case SequenceState.Running(u, i) => u || i
      case _                           => false
    }

    def userStopSet[F[_]](v: Boolean): State[F] => State[F] = status.modify {
      case r @ SequenceState.Running(_, _) => r.copy(userStop = v)
      case r                               => r
    }

    def internalStopSet[F[_]](v: Boolean): State[F] => State[F] = status.modify {
      case r @ SequenceState.Running(_, _) => r.copy(internalStop = v)
      case r                               => r
    }

    /**
      * Initialize a `State` passing a `Sequence` of pending `Step`s.
      */
    // TODO: Make this function `apply`?
    def init[F[_]](q: Sequence[F]): State[F] =
      Sequence.Zipper.zipper[F](q).map(Zipper(_, SequenceState.Idle, Map.empty))
        .getOrElse(Final(q, SequenceState.Idle))

    /**
      * Rebuilds the state of a sequence with a new steps definition, but preserving breakpoints and skip marks
      * The sequence must not be running.
      * @param steps New sequence definition
      * @param st Old sequence state
      * @return The new sequence state
      */
    def reload[F[_]](steps: List[Step[F]], st:State[F]): State[F] =
      if(st.status.isRunning) st
      else {
        val oldSeq = st.toSequence
        val updSteps = oldSeq.steps.zip(steps).map{ case (o, n) => n.copy(breakpoint =  o.breakpoint, skipMark = o.skipMark)} ++ steps.drop(oldSeq.steps.length)
        init(oldSeq.copy(steps = updSteps))
      }

    /**
      * This is the `State` in Zipper mode, which means is under execution.
      *
      */
    final case class Zipper[F[_]](zipper: Sequence.Zipper[F], status: SequenceState,
                                  singleRuns: Map[ActionCoordsInSeq, ActionState[F]]) extends
      State[F] { self =>

      override val next: Option[State[F]] = zipper.next match {
        // Last execution
        case None    => zipper.uncurrentify.map(Final[F](_, status))
        case Some(x) => Zipper(x, status, singleRuns).some
      }

      /**
        * Current Execution
        */
      override val current: Execution[F] =
        // Queue
        zipper
          // Step
          .focus
          // Execution
          .focus

      override val currentStep: Option[Step[F]] = zipper.focus.toStep.some

      override val pending: List[Step[F]] = zipper.pending

      override def rollback: Zipper[F] = self.copy(zipper = zipper.rollback)

      override def skips: Option[State[F]] = zipper.skips match {
        // Last execution
        case None    => zipper.uncurrentify.map(Final[F](_, status))
        case Some(x) => Zipper(x, status, singleRuns).some
      }

      override def setBreakpoint(stepId: StepId, v: Boolean): State[F] = self.copy(zipper =
        zipper.copy(pending =
          zipper.pending.map(s => if(s.id == stepId) s.copy(breakpoint = Step.BreakpointMark(v)) else s)))

      override def setSkipMark(stepId: StepId, v: Boolean): State[F] = self.copy(zipper =
        if(zipper.focus.id == stepId) zipper.copy(focus = zipper.focus.copy(skipMark = Step.SkipMark(v)))
        else zipper.copy(pending =
          zipper.pending.map(s => if(s.id == stepId) s.copy(skipMark = Step.SkipMark(v)) else s)))

      override def getCurrentBreakpoint: Boolean = zipper.focus.breakpoint.self && zipper.focus.done.isEmpty

      override val done: List[Step[F]] = zipper.done

      private val zipperL: Lens[Zipper[F], Sequence.Zipper[F]] =
        GenLens[Zipper[F]](_.zipper)

      override def mark(i: Int)(r: Result[F]): State[F] = {
        val currentExecutionL: Lens[Zipper[F], Execution[F]] = zipperL ^|-> Sequence.Zipper.current

        currentExecutionL.modify(_.mark(i)(r))(self)
      }

      override def start(i: Int): State[F] = {

        val currentExecutionL: Lens[Zipper[F], Execution[F]] = zipperL ^|-> Sequence.Zipper.current

        currentExecutionL.modify(_.start(i))(self).clearSingles
      }

      // Some rules:
      // 1. Done steps cannot change.
      // 2. Running step cannot change `done` or `focus` executions
      // 3. Must preserve breakpoints and skip marks
      override def update(stepDefs: List[List[ParallelActions[F]]]): State[F] =
        stepDefs.drop(zipper.done.length) match {
          case t :: ts => zipperL.modify(zp =>
            zp.copy(
              focus = zp.focus.update(t),
              pending = pending.zip(ts).map{case (step, exes) => step.copy(executions = exes)} ++ pending.drop(ts.length)
            )
          )(this)
          case _       => this
        }

      override val toSequence: Sequence[F] = zipper.toSequence

      override def startSingle(c: ActionCoordsInSeq): State[F] =
        if(zipper.done.find(_.id === c.stepId).isDefined)
          self
        else self.copy(singleRuns = singleRuns + (c -> ActionState.Started))

      override def failSingle(c: ActionCoordsInSeq, err: Result.Error): State[F] =
        if(getSingleState(c).started)
          self.copy(singleRuns = singleRuns + (c -> ActionState.Failed(err)))
        else
          self

      override def completeSingle[V <: RetVal](c: ActionCoordsInSeq, r: V): State[F] =
        if(getSingleState(c).started)
          self.copy(singleRuns = singleRuns + (c -> ActionState.Completed(r)))
        else
          self

      override def getSingleState(c: ActionCoordsInSeq): ActionState[F] =
        singleRuns.getOrElse(c, ActionState.Idle)

      override val getSingleActionStates: Map[ActionCoordsInSeq, ActionState[F]] = singleRuns

      override def getSingleAction(c: ActionCoordsInSeq): Option[Action[F]] = for {
        step <- toSequence.steps.find(_.id === c.stepId)
        exec <- step.executions.get(c.execIdx.self)
        act  <- exec.get(c.actIdx.self)
      } yield act

      override def clearSingles: State[F] = self.copy(singleRuns = Map.empty)
    }

    /**
      * Final `State`. This doesn't have any `Step` under execution, there are
      * only completed `Step`s.
      *
      */
    final case class Final[F[_]](seq: Sequence[F], status: SequenceState) extends State[F] { self =>

      override val next: Option[State[F]] = None

      override val current: Execution[F] = Execution.empty

      override val currentStep: Option[Step[F]] = none

      override val pending: List[Step[F]] = Nil

      override def rollback: Final[F] = self

      override def skips: Option[State[F]] = self.some

      override def setBreakpoint(stepId: StepId, v: Boolean): State[F] = self

      override def setSkipMark(stepId: StepId, v: Boolean): State[F] = self

      override def getCurrentBreakpoint: Boolean = false

      override val done: List[Step[F]] = seq.steps

      override def mark(i: Int)(r: Result[F]): State[F] = self

      override def start(i: Int): State[F] = self

      override def update(stepDefs: List[List[ParallelActions[F]]]): State[F] = self

      override val toSequence: Sequence[F] = seq

      override def startSingle(c: ActionCoordsInSeq): State[F] = self

      override def failSingle(c: ActionCoordsInSeq, err: Result.Error): State[F] = self

      override def completeSingle[V <: RetVal](c: ActionCoordsInSeq, r: V): State[F]
        = self

      override def getSingleState(c: ActionCoordsInSeq): ActionState[F] = ActionState.Idle

      override val getSingleActionStates: Map[ActionCoordsInSeq, ActionState[F]] = Map.empty

      override def getSingleAction(c: ActionCoordsInSeq): Option[Action[F]] = None

      override def clearSingles: State[F] = self
    }

  }

}
