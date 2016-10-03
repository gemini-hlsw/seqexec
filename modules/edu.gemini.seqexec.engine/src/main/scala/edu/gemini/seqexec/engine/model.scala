package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
 * Flag to indicate whether the global execution is `Running` or `Waiting`.
 */
sealed trait Status

object Status {
  case object Running extends Status
  case object Waiting extends Status
}

/**
  * A list of `Executions` grouped by observation.
  */
case class Step[A](id: Int, executions: NonEmptyList[Execution[A]])

object Step {

  def executions[A]: Step[A] @> NonEmptyList[Execution[A]] =
    Lens.lensu((s, exes) => s.copy(executions = exes), _.executions)

  def head[A]: Step[A] @> Execution[A] = executions >=> Lens.nelHeadLens

  /**
    * Adds a `Execution` to the front of a `Step`.
    */
  def cons[A](exe: Execution[A])(st: Step[A]): Step[A] = Step(st.id, exe <:: st.executions)

  /**
    * Return the next `Execution` and the remaining `Step` if there are more
    * `Execution`s left.
    */
  def uncons[A](st: Step[A]): (Execution[A], Option[Step[A]]) =
    (st.executions.head,
     st.executions.tail.toNel.map(Step(st.id, _))
    )

  implicit val stepFunctor = new Functor[Step] {
    def map[A, B](fa: Step[A])(f: A => B): Step[B] =
      Step(fa.id, fa.executions.map(_.map(f)))
  }
}

/**
  * A list of Steps grouped by target and instrument.
  */
case class Sequence[A](
  id: String,
  steps: NonEmptyList[Step[A]],
  // Unrooted executions
  executions: List[Execution[A]]
)

object Sequence {
  def steps[A]: Sequence[A] @> NonEmptyList[Step[A]] =
    Lens.lensu((s, sts) => s.copy(steps = sts), _.steps)

  // Next Step
  def head[A]: Sequence[A] @> Step[A] = steps >=> Lens.nelHeadLens

  /**
    * Adds an `Execution` to a `Sequence`.
    *
    * `Execution` wrappers:
    * - x \/ E: Adds it to the current `Step`.
    * - E \/ x: Creates a new `Step` ands adds it in the front of the `Sequence`.
    */
  def cons[A](exe2: (Execution[A], Int) \/ Execution[A])(seq: Sequence[A]): Sequence[A] =
    exe2 match {
      // Create new Step with the Execution
      case -\/((exe, sid)) =>
        Sequence(
          seq.id,
          Step(sid, NonEmptyList(exe, seq.executions: _*)) <:: seq.steps,
          Nil
        )
      // Add Execution to current Step
      case \/-(exe) => Sequence(seq.id, seq.steps, exe :: seq.executions)
    }

  /**
    * Returns the next `Execution` and the remaining of the `Sequence`.
    *
    * Execution wrappers:
    *
    * - x \/ (Execution, Sequence):
    *
    * Returns the next `Execution` and the remaining `Sequence` when there are
    * more `Execution`s left in the current `Step`.
    *
    * - (Execution, Option[Sequence]) \/ x:
    *
    * Returns the next `Execution` and the remaining `Sequence` when there are
    * no more `Execution`s in the current `Step`. If this was the last `Step` of
    * the current `Sequence` it becomes `None`.
    */
  // TODO: Handle unrooted executions
  def uncons[A](seq: Sequence[A]): (((Execution[A], Int), Option[Sequence[A]]) \/ (Execution[A], Sequence[A])) = {
    val (exe, mstep) = Step.uncons(seq.steps.head)
    mstep match {
      // No more Executions in Step, remove Step
      case None => (
        (exe, seq.steps.head.id),
        seq.steps.tail.toNel.map(Sequence(seq.id, _, seq.executions))
      ).left
      // More Executions in Step, remove Execution in Step
      case Some(st) => (exe, head.set(seq, st)).right
    }
  }

  implicit val SequenceFunctor = new Functor[Sequence] {
    def map[A, B](fa: Sequence[A])(f: A => B): Sequence[B] =
      Sequence(fa.id, fa.steps.map(_.map(f)), fa.executions.map(_.map(f)))
  }
}

/**
  * A list of Sequences. The `Queue` could be empty of Sequences when waiting
  * for the addition of new ones.
  */
case class Queue[A](
  sequences: List[Sequence[A]],
  // Unrooted Steps (without a Queue)
  steps: List[Step[A]],
  // Unrooted Executions (without a Step)
  executions: List[Execution[A]]
) {

  def isEmpty: Boolean = sequences.isEmpty

}

object Queue {

  // TODO: Replace this with `Either3`
  type Execution3[A] = (Execution[A], String, Int) \/ (Execution[A], Int) \/ Execution[A]

  def empty[A]: Queue[A] = Queue(Nil, Nil, Nil)

  def sequences[A]: Queue[A] @> List[Sequence[A]] =
    Lens.lensu((q, s) => q.copy(sequences = s), _.sequences)

  def steps[A]: Queue[A] @> List[Step[A]] =
    Lens.lensu((q, s) => q.copy(steps = s), _.steps)

  def executions[A]: Queue[A] @> List[Execution[A]] =
    Lens.lensu((q, e) => q.copy(executions = e), _.executions)

  // Next `Sequence`
  def head[A]: Queue[A] @?> Sequence[A] = sequences.partial >=> PLens.listHeadPLens

  /**
    * Adds an `Execution` to the `Queue`.
    *
    * `Execution` wrappers:
    * - x \/ x \/ E:
    *
    * Adds `Execution` to the current `Step`.
    *
    * - x \/ E \/ x:
    *
    * Creates a new `Step` with the given `Execution` and adds it to the current
    * `Sequence`.
    *
    * - E \/ x \/ x:
    * Creates a new `Sequence` with a new `Step` with the given `Execution` and
    * adds it in the front of the `Queue`.
    */
  def cons[A](exe3: Execution3[A])(q: Queue[A]): Queue[A] =
    exe3 match {
      // New Sequence
      case -\/(-\/((exe, seqid, stepid))) =>
        Queue(
          Sequence(
            seqid,
            NonEmptyList(
              Step(
                stepid,
                NonEmptyList(
                  exe,
                  q.executions: _*
                )
              ),
              q.steps: _*
            ),
            Nil
          ) :: q.sequences,
          Nil,
          Nil
        )
      // New Step
      case -\/(\/-((exe, i))) => {
        executions.set(
          steps.mod(
            Step(i, NonEmptyList(exe, q.executions: _*)) :: _,
            q
          ),
          Nil
        )
      }
      // Modify current Step
      case \/-(exe) => executions.mod(exe :: _, q)
    }

  /**
    * Returns the next `Execution` and the remaining `Queue`. If the `Execution`
    * was the last one the `Queue` becomes empty.
    *
    * `Execution` wrappers:
    *  - x \/ x \/ E: When more `Execution`s remain in the current `Step`.
    *  - x \/ E \/ x: When current `Step` has been completed.
    *  - E \/ x \/ x: When current `Sequence` has been completed.
    *
    * `uncons`ing on an empty `Queue` returns `None`.
    */
  // TODO: Handle unrooted Sequences and Steps
  def uncons[A](q: Queue[A]): Option[(Execution3[A], Queue[A])] =
    // Queue empty?
    q.sequences.headOption.map(seq0 =>
      Sequence.uncons(seq0) match {
        // Current Step completed
        case -\/(((exe, stepid), mseq)) => {
          mseq match {
            // No more Steps in current Sequence, remove Sequence.
            // TODO: listTailPLens?
            case None => (
              (exe, seq0.id, stepid).left.left,
              Queue(q.sequences.tailOption.getOrElse(Nil),
                    q.steps,
                    q.executions)
            )
            // More Steps left in current Sequence, remove `Step` from Sequence.
            case Some(seq) => ((exe, stepid).right.left, head.set(q, seq).getOrElse(q))
          }
        }
        // Executions remaining in current Step
        case \/-((exe, seq)) => {
          // `Execution` already extracted in seq
          (exe.right, head.set(q, seq).getOrElse(q))
        }
      }
    )

  // TODO: This violates Monoid laws, but after removing Queue.steps and //
  // Queue.executions it should abide out-of-the-box.
  implicit def queueMonoid[A]: Monoid[Queue[A]] = new Monoid[Queue[A]] {
    def append(a: Queue[A], b: => Queue[A]): Queue[A] =
      Queue(a.sequences ::: b.sequences, b.steps, b.executions)

    val zero: Queue[A] = Queue(Nil, Nil, Nil)

  }

  implicit val queueFunctor = new Functor[Queue] {
    def map[A, B](fa: Queue[A])(f: A => B): Queue[B] =
      Queue(fa.sequences.map(_.map(f)),
            fa.steps.map(_.map(f)),
            fa.executions.map(_.map(f))
      )
  }
}

/**
 * This is the main state data type to be used by the `Engine`. This is what
 * gets modified whenever it needs to react to an Event.
 */
case class QState(pending: Queue[Action],
                  current: Current,
                  done: Queue[Result],
                  status: Status) {

  def isEmpty: Boolean = pending.sequences.isEmpty && current.isEmpty

  def output: Queue[Action \/ Result] =
    // Type inference needs some help
    // TODO: Reverse done Sequences? It depends on what's more convenient to the client
    // XXX: Include Current execution
    (done.map(_.right): Queue[Action \/ Result]) |+| pending.map(_.left)
}

object QState {

  val pending: QState @> Queue[Action] =
    Lens.lensu((s, q) => s.copy(pending = q), _.pending)

  val current: QState @> Current =
    Lens.lensu((s, c) => s.copy(current = c), _.current)

  val done: QState @> Queue[Result] =
    Lens.lensu((s, q) => s.copy(done = q), _.done)

  val status: QState @> Status =
    Lens.lensu((s, st) => s.copy(status = st), _.status)

  val empty: QState = QState(Queue.empty, Current.empty, Queue.empty, Status.Waiting)

  /**
    * Initialize a `QState` passing a `Queue` of `Action`s. This also takes care
    * of making the first pending `Execution` `Current`.
    */
  def init(q: Queue[Action]): QState = pending.set(empty, q)

  /**
    * Given an index of a current `Action` it replaces such `Action` with the
    * `Result` and returns the new modified `State`.
    *
    * If after marking the `Action`, all elements in `Current` are `Result`, it
    * empties `Current` and moves the `Result` to the completed `Queue`
    *
    * If the index doesn't exist, the new `State` is returned unmodified.
    */
  def mark(i: Int)(r: Result)(qs: QState): QState =
    current.mod(Current.mark(i)(r)(_), qs)

  /**
    * Returns a new `State` where the next pending `Execution` has been promoted
    * to `Current` and `Current` is placed in the completed `Queue`. As a
    * convenience it also returns the `Execution` with pending `Actions` just
    * before making it `Current`.
    *
    * If the `Current` doesn't have all actions completed or there are no more
    * pending `Execution`s it returns None.
    */
  def next(qs: QState): Option[QState] = cleanup(qs).flatMap(prime)

  // None: current not empty
  //       pending queue empty
  def prime(qs: QState): Option[QState] =
    if (qs.current.isEmpty)
      Queue.uncons(qs.pending).map {
        case (exe3, q) => QState(q, currentify(exe3), qs.done, qs.status)
      }
    else None

  def cleanup(qs: QState): Option[QState] =
    uncurrentify(qs.current).map(exe3 =>
      QState(qs.pending, Current.empty, Queue.cons(exe3)(qs.done), qs.status)
    )
  /**
    * Transform an *unconsed* pending `Execution` into `Current` in addition to
    * returning the unwrapped `Execution`.
    */
  // TODO: Use same structure for `Current` and `Queue.Execution3`?
  private def currentify(exe3: Queue.Execution3[Action]): Current = {

    def vec(exe: Execution[Action]): Vector[Action \/ Result] = exe.map(_.left).toVector

    exe3 match {
      // New Sequence
      case -\/(-\/((actions, seqid, stepid))) => Current(vec(actions), Some((seqid, stepid).left))
      // New Step
      case -\/(\/-((actions, stepid))) => Current(vec(actions), Some(stepid.right))
      // Modify current Step
      case \/-(actions) => Current(vec(actions), None)
    }
  }

  /**
    * Transform a `Current` into a completed `Execution` for *consing*. If there
    * is any pending `Action` or no `Result`s in `Current` it returns None.
    */
  // TODO: Use same structure for `Current` and `Queue.Execution3`?
  private def uncurrentify(current: Current): Option[Queue.Execution3[Result]] = {

    val unvec: Option[Execution[Result]] =
      if (current.ars.all(_.isRight)) current.results.toNel
      else None

    unvec.map(
      exe => current.ctxt match {
        // Modify current Step
        case None => exe.right
        // New Sequence
        case Some(-\/((seqid, stepid))) => (exe, seqid, stepid).left.left
        // New Step
        case Some(\/-(stepid)) => (exe, stepid).right.left
      }
    )
  }
}

/**
  * This structure holds the `Execution` currently under execution. It carries
  * information about which `Action`s have been completed and the position in
  * the `Queue` for proper insertion into the completed `Queue` when all the
  * `Execution`s are done.
  */
case class Current(
  ars:  Vector[Action \/ Result],
  ctxt: Option[(String, Int) \/ Int]
) {

  def isEmpty: Boolean = ars.empty

  def actions: List[Action] = {

    def lefts[L, R](xs: List[L \/ R]): List[L] = xs.collect { case -\/(l) => l }

    lefts(ars.toList)
  }

  def results: List[Result] = {

    def rights[L, R](xs: List[L \/ R]): List[R] = xs.collect { case \/-(r) => r }

    rights(ars.toList)
  }
}

object Current {
  val empty: Current = Current(Vector.empty, None)
  /**
    * Set the `Result` for the given `Action` index in `Current`.
    *
    * If the index doesn't exist, `Current` is returned unmodified.
    */
  def mark(i: Int)(r: Result)(c: Current): Current =
    Current(PLens.vectorNthPLens(i).setOr(c.ars, r.right, c.ars), c.ctxt)
}

/**
  * The result of an `Action`.
  */
sealed trait Result

object Result {
  case class OK[R](r: R) extends Result
  case class Error[E](e: E) extends Result
}
