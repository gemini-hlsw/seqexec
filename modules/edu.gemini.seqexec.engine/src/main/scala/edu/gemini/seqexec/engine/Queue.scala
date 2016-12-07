package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * A list of `Sequence`s.
  */
case class Queue[+A](sequences: List[Sequence[A]])

object Queue {

  type Id = String

  def sequences[A]: Queue[A] @> List[Sequence[A]] =
    Lens.lensu((q, s) => q.copy(sequences = s), _.sequences)

  implicit def queueMonoid[A]: Monoid[Queue[A]] = new Monoid[Queue[A]] {
    def append(a: Queue[A], b: => Queue[A]): Queue[A] =
      Queue(a.sequences ::: b.sequences)

    val zero: Queue[A] = Queue(Nil)
  }

  implicit val queueFunctor = new Functor[Queue] {
    def map[A, B](fa: Queue[A])(f: A => B): Queue[B] =
      Queue(fa.sequences.map(_.map(f)))
  }

  /**
    * Queue Zipper. This structure is optimized for the actual `Queue`
    * execution.
    *
    */
  case class Zipper(
    pending: List[Sequence[Action]],
    focus: Sequence.Zipper,
    done: List[Sequence[Result]]
  ) {

    /**
      * Runs the next execution. If the current `Sequence` is completed it adds
      * the `Sequence.Zipper` under focus to the list of completed `Sequence`s and makes
      * the next pending `Sequence` the current one.
      *
      * If there are still `Step`s that have not finished in the current
      * `Sequence` or if there are no more pending `Seqeunce`s it returns `None`.
      */
    val next: Option[Zipper] =
      focus.next match {
        // Sequence completed
        case None       =>
          pending match {
            case Nil           => None
            case seqp :: seqps => for {
              curr <- Sequence.Zipper.currentify(seqp)
              seqd <- focus.uncurrentify
            } yield Zipper(seqps, curr, seqd :: done)
          }
        // Current Sequence ongoing
        case Some(seqz) => Some(Zipper(pending, seqz, done))
      }

    /**
      * Obtain the `Queue` results only if all `Step`s have been completed.
      * This is a special way of *unzipping* a `Sequence.Zipper`.
      *
      */
    val uncurrentify: Option[Queue[Result]] =
      if (pending.isEmpty) focus.uncurrentify.map(x => Queue(x :: done))
      else None

    /**
      * Unzip `Zipper`. This creates a single `Sequence` with either completed
      * `Step`s or pending `Step`s.
      */
    val toQueue: Queue[Action \/ Result] =
      Queue(
        done.map(_.map(_.right)) ++
        List(focus.toSequence) ++
        pending.map(_.map(_.left))
      )

  }

  object Zipper {

    /**
      * Make a `Zipper` from a `Queue` only if all the `Sequence`s in the
      * `Queue` are pending. This is a special way of *zipping* a `Queue`.
      *
      */
    def currentify(queue: Queue[Action]): Option[Zipper] =
      queue.sequences match {
        case Nil         => None
        case seq :: seqs =>
          Sequence.Zipper.currentify(seq).map(
            Zipper(seqs, _, Nil)
          )
      }

    private val focus: Zipper @> Sequence.Zipper =
      Lens.lensu((q, f) => q.copy(focus = f), _.focus)

    val current: Zipper @> Execution = focus >=> Sequence.Zipper.current

  }

  /**
    * This is the top level state exposed by the `Engine`. This is what gets
    * generated whenever it needs to react to an `Event`.
    */
  sealed trait State {

    /**
      * Returns a new `State` where the next pending `Sequence` is been made the
      * current `Sequence` under execution and the previous current `Sequence` is
      * placed in the completed `Queue`.
      *
      * If the current `Sequence` has `Step`s not completed or there are no more
      * pending `Seqeunce`s it returns `None`.
      */
    val next: Option[State]

    val status: Status

    val pending: List[Sequence[Action]]

    /**
      * Current Execution
      */
    val current: Execution

    val done: List[Sequence[Result]]

    /**
      * Given an index of a current `Action` it replaces such `Action` with the
      * `Result` and returns the new modified `State`.
      *
      * If the index doesn't exist, the new `State` is returned unmodified.
      */
    def mark(i: Int)(r: Result): State

    /**
      * Unzip `State`. This creates a single `Queue` with either completed `Sequence`s
      * or pending `Sequence`s.
      */
    val toQueue: Queue[Action \/ Result]

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
    def init(q: Queue[Action]): State = Initial(q, Status.Waiting)


    /**
      * Initial `State`. This doesn't have any `Sequence` under execution, there are
      * only pending `Step`s.
      *
      */
    case class Initial(queue: Queue[Action], status: Status) extends State { self =>

      val next: Option[State] =
        Queue.Zipper.currentify(queue).map(Zipper(_, status))

      val pending: List[Sequence[Action]] = queue.sequences

      val current: Execution = Execution.empty

      val done: List[Sequence[Result]] = Nil

      def mark(i: Int)(r: Result): State = self

      val toQueue: Queue[Action \/ Result] = queue.map(_.left)

    }

    /**
      * This is the `State` in Zipper mode, which means is under execution.
      *
      */
    case class Zipper(zipper: Queue.Zipper, status: Status) extends State { self =>

      val next: Option[State] = zipper.next match {
        // Last execution
        case None    => zipper.uncurrentify.map(Final(_, status))
        case Some(x) => Some(Zipper(x, status))
      }

      /**
        * Current Execution
        */
      val current: Execution =
        // Queue
        zipper
          // Sequence
          .focus
          // Step
          .focus
          // Execution
          .focus

      val pending: List[Sequence[Action]] = zipper.pending

      val done: List[Sequence[Result]] = zipper.done

      def mark(i: Int)(r: Result): State = {

        val zipper: Zipper @> Queue.Zipper =
          Lens.lensu((qs, z) => qs.copy(zipper = z), _.zipper)

        val current: Zipper @> Execution = zipper >=> Queue.Zipper.current

        current.mod(_.mark(i)(r), self)

      }

      val toQueue: Queue[Action \/ Result] =
        Queue(
          done.map(_.map(_.right)) ++
          List(zipper.focus.toSequence) ++
          pending.map(_.map(_.left))
        )

    }

    /**
      * Final `State`. This doesn't have any `Sequence` under execution, there are
      * only completed `Step`s.
      *
      */
    case class Final(queue: Queue[Result], status: Status) extends State { self =>

      val next: Option[State] = None

      val current: Execution = Execution.empty

      val pending: List[Sequence[Action]] = Nil

      val done: List[Sequence[Result]] = queue.sequences

      def mark(i: Int)(r: Result): State = self

      val toQueue: Queue[Action \/ Result] = queue.map(_.right)

    }

  }

}
