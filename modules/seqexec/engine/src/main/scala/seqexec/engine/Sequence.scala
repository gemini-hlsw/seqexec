// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import seqexec.model.{SequenceState, StepState}
import gem.Observation
import cats.implicits._
import monocle.Lens
import monocle.macros.GenLens

/**
  * A list of `Step`s grouped by target and instrument.
  */
final case class Sequence(
  id: Observation.Id,
  steps: List[Step]
)

object Sequence {

  def empty[A](id: Observation.Id): Sequence = Sequence(id, Nil)

  /**
    * Sequence Zipper. This structure is optimized for the actual `Sequence`
    * execution.
    *
    */
  final case class Zipper(
    id: Observation.Id,
    pending: List[Step],
    focus: Step.Zipper,
    done: List[Step]
  ) {

    private val (toSkip, remaining): (List[Step], List[Step]) = pending.span(st => st.skipMark.self)

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
    val next: Option[Zipper] =
      focus.next match {
        // Step completed
        case None      =>
          val (toSkip, remaining): (List[Step], List[Step]) = pending.span(st => st.skipMark.self && !st.breakpoint.self)
          remaining match {
            case Nil => None
            case stepp :: stepps => (Step.Zipper.currentify(stepp), focus.uncurrentify).mapN (
              (curr, stepd) => Zipper(id, stepps, curr, (done :+ stepd) ::: toSkip.map(_.copy(skipped = Step.Skipped(true))))
            )
          }
        // Current step ongoing
        case Some(stz) => Some(Zipper(id, pending, stz, done))
      }

    def rollback: Zipper = this.copy(focus = focus.rollback)

    //Skips steps before starting a sequence.
    def skips: Option[Zipper] = {
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
    val uncurrentify: Option[Sequence] =
      if (remaining.isEmpty)
        if(focus.skipMark.self) Sequence(id, (done :+ focus.skip) ::: toSkip.map(_.copy(skipped = Step.Skipped(true)))).some
        else focus.uncurrentify.map(x => Sequence(id, (done :+ x) ::: toSkip.map(_.copy(skipped = Step.Skipped(true)))))
      else None

    /**
      * Unzip a `Zipper`. This creates a single `Sequence` with either
      * completed `Step`s or pending `Step`s.
      */
    val toSequence: Sequence =
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
    def currentify(seq: Sequence): Option[Zipper] =
      seq.steps match {
        case Nil           => None
        case step :: steps =>
          Step.Zipper.currentify(step).map(
            Zipper(seq.id, steps, _, Nil)
          )
      }

    def zipper(seq: Sequence): Option[Zipper] =
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
    private def separate(seq: Sequence): Option[(List[Step], List[Step])] = {

      seq.steps.foldLeftM[Option, (List[Step], List[Step])]((Nil, Nil))(
        (acc, step) =>
        if (Step.status(step) === StepState.Pending)
          Some(acc.leftMap(_ :+ step))
        else if (Step.status(step) === StepState.Completed || Step.status(step) === StepState.Skipped)
          Some(acc.map(_ :+ step))
        else None
      )

    }

    val focus: Lens[Zipper, Step.Zipper] =
      GenLens[Zipper](_.focus)

    val current: Lens[Zipper, Execution] =
      focus ^|-> Step.Zipper.current

  }

  sealed trait State {

    /**
      * Returns a new `State` where the next pending `Step` is been made the
      * current `Step` under execution and the previous current `Step` is
      * placed in the completed `Sequence`.
      *
      * If the current `Step` has `Execution`s not completed or there are no more
      * pending `Step`s it returns `None`.
      */
    val next: Option[State]

    val status: SequenceState

    val pending: List[Step]

    def rollback: State

    def skips: Option[State]

    def setBreakpoint(stepId: Step.Id, v: Boolean): State

    def setSkipMark(stepId: Step.Id, v: Boolean): State

    def getCurrentBreakpoint: Boolean

    /**
      * Current Execution
      */
    val current: Execution

    val done: List[Step]

    /**
      * Given an index of a current `Action` it replaces such `Action` with the
      * `Result` and returns the new modified `State`.
      *
      * If the index doesn't exist, the new `State` is returned unmodified.
      */
    def mark(i: Int)(r: Result): State

    def start(i: Int): State

    def update(sequence: Sequence): State

    /**
      * Unzip `State`. This creates a single `Sequence` with either completed `Step`s
      * or pending `Step`s.
      */
    val toSequence: Sequence

  }

  object State {

    val status: Lens[State, SequenceState] =
    // `State` doesn't provide `.copy`
      Lens[State, SequenceState](_.status)(s => {
        case Zipper(st, _) => Zipper(st, s)
        case Final(st, _)  => Final(st, s)
      })

    def isRunning(st: State): Boolean = st.status.isRunning

    def userStopRequested(st: State): Boolean = st.status.userStopRequested

    def anyStopRequested(st: State): Boolean = st.status match {
      case SequenceState.Running(u, i) => u || i
      case _                           => false
    }

    def userStopSet(v: Boolean): State => State = status.modify {
      case r@SequenceState.Running(_, _) => r.copy(userStop = v)
      case r                             => r
    }

    def internalStopSet(v: Boolean): State => State = status.modify {
      case r@SequenceState.Running(_, _) => r.copy(internalStop = v)
      case r                             => r
    }

    /**
      * Initialize a `State` passing a `Queue` of pending `Sequence`s.
      */
    // TODO: Make this function `apply`?
    def init(q: Sequence): State =
      Sequence.Zipper.zipper(q).map(Zipper(_, SequenceState.Idle))
        .getOrElse(Final(Sequence.empty(q.id), SequenceState.Idle))

    /**
      * This is the `State` in Zipper mode, which means is under execution.
      *
      */
    final case class Zipper(zipper: Sequence.Zipper, status: SequenceState) extends State { self =>

      override val next: Option[State] = zipper.next match {
        // Last execution
        case None    => zipper.uncurrentify.map(Final(_, status))
        case Some(x) => Zipper(x, status).some
      }

      /**
        * Current Execution
        */
      override val current: Execution =
        // Queue
        zipper
          // Step
          .focus
          // Execution
          .focus

      override val pending: List[Step] = zipper.pending

      override def rollback: Zipper = self.copy(zipper = zipper.rollback)

      override def skips: Option[State] = zipper.skips match {
        // Last execution
        case None    => zipper.uncurrentify.map(Final(_, status))
        case Some(x) => Zipper(x, status).some
      }

      override def setBreakpoint(stepId: Step.Id, v: Boolean): State = self.copy(zipper =
        zipper.copy(pending =
          zipper.pending.map(s => if(s.id == stepId) s.copy(breakpoint = Step.BreakpointMark(v)) else s)))

      override def setSkipMark(stepId: Step.Id, v: Boolean): State = self.copy(zipper =
        if(zipper.focus.id == stepId) zipper.copy(focus = zipper.focus.copy(skipMark = Step.SkipMark(v)))
        else zipper.copy(pending =
          zipper.pending.map(s => if(s.id == stepId) s.copy(skipMark = Step.SkipMark(v)) else s)))

      override def getCurrentBreakpoint: Boolean = zipper.focus.breakpoint.self && zipper.focus.done.isEmpty

      override val done: List[Step] = zipper.done

      private val zipperL: Lens[Zipper, Sequence.Zipper] =
        GenLens[Zipper](_.zipper)

      override def mark(i: Int)(r: Result): State = {

        val currentExecutionL: Lens[Zipper, Execution] = zipperL ^|-> Sequence.Zipper.current

        val currentFileIdL: Lens[Zipper, Option[FileId]] =
          zipperL ^|-> Sequence.Zipper.focus ^|-> Step.Zipper.fileId

        val z: Zipper = r match {
            case Result.Partial(Result.FileIdAllocated(fileId), _) => currentFileIdL.set(fileId.some)(self)
            case _                                                 => self
          }

        currentExecutionL.modify(_.mark(i)(r))(z)

      }

      override def start(i: Int): State = {

        val currentExecutionL: Lens[Zipper, Execution] = zipperL ^|-> Sequence.Zipper.current

        currentExecutionL.modify(_.start(i))(self)

      }

      // Some rules:
      // 1. Done steps cannot change.
      // 2. Pending steps cannot turn to not pending.
      // 3. Running step cannot change done or focus executions
      // 4. Must preserve breakpoints and skip marks
      override def update(sequence: Sequence): State = {
        val updatedPending = sequence.steps.drop(zipper.done.length)

        require(updatedPending.forall(Step.status(_) === StepState.Pending))

        updatedPending match {
          case t::ts => zipperL.modify(zp => zp.copy(focus = zp.focus.update(t), pending = pending.zip(ts).map{
            case (o, n) => n.copy(breakpoint = o.breakpoint, skipMark = o.skipMark)
          } ++ ts.drop(pending.length)))(this)
          case _     => if(status.isRunning) this
                        else Final(Sequence(zipper.id, zipper.done), status)
        }

      }

      override val toSequence: Sequence = zipper.toSequence

    }

    /**
      * Final `State`. This doesn't have any `Step` under execution, there are
      * only completed `Step`s.
      *
      */
    final case class Final(seq: Sequence, status: SequenceState) extends State { self =>

      override val next: Option[State] = None

      override val current: Execution = Execution.empty

      override val pending: List[Step] = Nil

      override def rollback: Final = self

      override def skips: Option[State] = self.some

      override def setBreakpoint(stepId: Step.Id, v: Boolean): State = self

      override def setSkipMark(stepId: Step.Id, v: Boolean): State = self

      override def getCurrentBreakpoint: Boolean = false

      override val done: List[Step] = seq.steps

      override def mark(i: Int)(r: Result): State = self

      override def start(i: Int): State = self

      override def update(sequence: Sequence): State = self

      override val toSequence: Sequence = seq

    }

  }

}
