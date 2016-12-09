package edu.gemini.seqexec.engine

import edu.gemini.seqexec.model.SharedModel.{SequenceMetadata, SequenceState}

import scalaz._
import Scalaz._

/**
  * A list of `Step`s grouped by target and instrument.
  */
case class Sequence[+A](id: String, metadata: SequenceMetadata, steps: List[Step[A]])

object Sequence {

  type Id = String

  def empty[A](id: String): Sequence[A] = Sequence(id, SequenceMetadata(""), Nil)

  /**
    * Calculate the `Sequence` `Status` based on the underlying `Action`s.
    *
    */
  def status(seq: Sequence[Action \/ Result]): SequenceState = {

    // TODO: Get rid of this
    if (seq.steps.isEmpty || seq.steps.all(_.executions.all(_.isEmpty))
          || seq.any(Execution.errored)
    ) SequenceState.Error("An action errored")
    else if (seq.all(_.isLeft)) SequenceState.Idle
    else if (seq.all(_.isRight)) SequenceState.Completed
    else SequenceState.Running
  }

  implicit val SequenceFunctor = new Functor[Sequence] {
    def map[A, B](fa: Sequence[A])(f: A => B): Sequence[B] =
      Sequence(fa.id, fa.metadata, fa.steps.map(_.map(f)))
  }

  // TODO: Proof Foldable laws
  implicit val stepFoldable = new Foldable[Sequence] {
    def foldMap[A, B](fa: Sequence[A])(f: A => B)(implicit F: scalaz.Monoid[B]): B =
      // TODO: Foldable composition?
      fa.steps.foldMap(_.foldMap(f))

    def foldRight[A, B](fa: Sequence[A], z: => B)(f: (A, => B) => B): B =
      fa.steps.foldRight(z)((l, b) => l.foldRight(b)(f(_, _)))
  }

  /**
    * Sequence Zipper. This structure is optimized for the actual `Sequence`
    * execution.
    *
    */
  case class Zipper(
    id: String,
    metadata: SequenceMetadata,
    pending: List[Step[Action]],
    focus: Step.Zipper,
    done: List[Step[Result]]
  ) {

    /**
      * Runs the next execution. If the current `Step` is completed it adds the
      * `StepZ` under focus to the list of completed `Step`s and makes the next
      * pending `Step` the current one.
      *
      * If there are still `Execution`s that have not finished in the current
      * `Step` or if there are no more pending `Step`s it returns `None`.
      */
    val next: Option[Zipper] =
      focus.next match {
        // Step completed
        case None      =>
          pending match {
            case Nil             => None
            case stepp :: stepps =>
              (Step.Zipper.currentify(stepp) |@| focus.uncurrentify) (
                (curr, stepd) => Zipper(id, metadata, stepps, curr, stepd :: done)
              )
          }
        // Current step ongoing
        case Some(stz) => Some(Zipper(id, metadata, pending, stz, done))
      }

    /**
      * Obtain the resulting `Sequence` only if all `Step`s have been completed.
      * This is a special way of *unzipping* a `Zipper`.
      *
      */
    val uncurrentify: Option[Sequence[Result]] =
      if (pending.isEmpty) focus.uncurrentify.map(x => Sequence(id, metadata, x :: done))
      else None

    /**
      * Unzip a `Zipper`. This creates a single `Sequence` with either
      * completed `Step`s or pending `Step`s.
      */
    val toSequence: Sequence[Action \/ Result] =
      Sequence(
        id,
        metadata,
        // TODO: Functor composition?
        done.map(_.map(_.right)) ++
          List(focus.toStep) ++
          pending.map(_.map(_.left))
      )
  }

  object Zipper {

    /**
      * Make a `Zipper` from a `Sequence` only if all the `Step`s in the
      * `Sequence` are pending. This is a special way of *zipping* a `Sequence`.
      *
      */
    def currentify(seq: Sequence[Action]): Option[Zipper] =
      seq.steps match {
        case Nil           => None
        case step :: steps =>
          Step.Zipper.currentify(step).map(
            Zipper(seq.id, seq.metadata, steps, _, Nil)
          )
      }

    private val focus: Zipper @> Step.Zipper =
      Lens.lensu((s, f) => s.copy(focus = f), _.focus)

    val current: Zipper @> Execution = focus >=> Step.Zipper.current

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

    val status: Status

    val pending: List[Step[Action]]

    /**
      * Current Execution
      */
    val current: Execution

    val done: List[Step[Result]]

    /**
      * Given an index of a current `Action` it replaces such `Action` with the
      * `Result` and returns the new modified `State`.
      *
      * If the index doesn't exist, the new `State` is returned unmodified.
      */
    def mark(i: Int)(r: Result): State

    /**
      * Unzip `State`. This creates a single `Sequence` with either completed `Step`s
      * or pending `Step`s.
      */
    val toSequence: Sequence[Action \/ Result]

  }

  object State {

    val status: State @> Status =
    // `State` doesn't provide `.copy`
      Lens.lensu(
        (qs, s) => (
          qs match {
            // TODO: Isn't there a better way to write this?
            case Initial(st, _) => Initial(st, s)
            case Zipper(st, _) => Zipper(st, s)
            case Final(st, _) => Final(st, s)
          }
          ),
        _.status
      )

    /**
      * Initialize a `State` passing a `Queue` of pending `Sequence`s.
      */
    // TODO: Make this function `apply`?
    def init(q: Sequence[Action]): State = Initial(q, Status.Waiting)


    /**
      * Initial `State`. This doesn't have any `Sequence` under execution, there are
      * only pending `Step`s.
      *
      */
    case class Initial(seq: Sequence[Action], status: Status) extends State {
      self =>

      override val next: Option[State] =
        Sequence.Zipper.currentify(seq).map(Zipper(_, status))

      override val pending: List[Step[Action]] = seq.steps

      override val current: Execution = Execution.empty

      override val done: List[Step[Result]] = Nil

      override def mark(i: Int)(r: Result): State = self

      override val toSequence: Sequence[Action \/ Result] = seq.map(_.left)

    }
    /**
      * This is the `State` in Zipper mode, which means is under execution.
      *
      */
    case class Zipper(zipper: Sequence.Zipper, status: Status) extends State { self =>

      override val next: Option[State] = zipper.next match {
        // Last execution
        case None    => zipper.uncurrentify.map(Final(_, status))
        case Some(x) => Some(Zipper(x, status))
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

      override val pending: List[Step[Action]] = zipper.pending

      override val done: List[Step[Result]] = zipper.done

      override def mark(i: Int)(r: Result): State = {

        val zipper: Zipper @> Sequence.Zipper =
          Lens.lensu((qs, z) => qs.copy(zipper = z), _.zipper)

        val current: Zipper @> Execution = zipper >=> Sequence.Zipper.current

        current.mod(_.mark(i)(r), self)

      }

      override val toSequence: Sequence[Action \/ Result] = zipper.toSequence

    }

    /**
      * Final `State`. This doesn't have any `Sequence` under execution, there are
      * only completed `Step`s.
      *
      */
    case class Final(seq: Sequence[Result], status: Status) extends State { self =>

      override val next: Option[State] = None

      override val current: Execution = Execution.empty

      override val pending: List[Step[Action]] = Nil

      override val done: List[Step[Result]] = seq.steps

      override def mark(i: Int)(r: Result): State = self

      override val toSequence: Sequence[Action \/ Result] = seq.map(_.right)

    }


  }

}
