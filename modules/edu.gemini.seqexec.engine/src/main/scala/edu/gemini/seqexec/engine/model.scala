package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
 * This is the main state data type to be used by the `Engine`. This is what
 * gets modified whenever it needs to react to an Event.
 */
case class State(pending: Queue[Action],
                 current: Current,
                 done: Queue[Result],
                 status: Status)

object State {

  val pending: State @> Queue[Action] =
    Lens.lensu((s, q) => s.copy(pending = q), _.pending)

  val current: State @> Current =
    Lens.lensu((s, c) => s.copy(current = c), _.current)

  val done: State @> Queue[Result] =
    Lens.lensu((s, q) => s.copy(done = q), _.done)

  val status: State @> Status =
    Lens.lensu((s, st) => s.copy(status = st), _.status)

  def mark(i: Int)(r: Result)(st: State): State =
    current.mod(Current.mark(i)(r)(_), st)

  /**
    * Returns a new `State` where the next pending `Execution` has been promoted
    * to `Current` and `Current` is placed in the completed `Queue`. As a
    * convenience it also returns the `Execution` with pending `Actions` just
    * before making it `Current`.
    *
    * If the `Current` doesn't have all actions completed or there are no more
    * pending `Execution`s it returns None.
    */
  def next(st: State): Option[(Execution[Action], State)] = for {
    exe3done <- uncurrentify(st.current)
    qd = Queue.cons(exe3done)(st.done)
    (exe3pending, qp) <- Queue.uncons(st.pending)
    (actions, c) = currentify(exe3pending)
  } yield (actions, State(qp, c, qd, st.status))

  /**
    * Transform an *unconsed* pending `Execution` into `Current` in addition to
    * returning the unwrapped `Execution`.
    */
  // TODO: Use same structure for `Current` and `Queue.Execution3`?
  private def currentify(exe3: Queue.Execution3[Action]): (Execution[Action], Current) = {

    def vec(exe: Execution[Action]): Vector[Action \/ Result] = exe.map(_.left).toVector

    exe3 match {
      case -\/(-\/((actions, seqid, stepid))) => (actions, Current(vec(actions), Some((seqid, stepid).left)))
      case -\/(\/-((actions, stepid))) => (actions, Current(vec(actions), Some(stepid.right)))
      case \/-(actions) => (actions, Current(vec(actions), None))
    }
  }

  /**
    * Transform a `Current` into a completed `Execution` for *consing*. If there
    * is any pending `Action` or no `Result`s in `Current` it returns None.
    */
  // TODO: Use same structure for `Current` and `Queue.Execution3`?
  private def uncurrentify(current: Current): Option[Queue.Execution3[Result]] = {

    // not available in scalaz?
    def rights[L, R](xs: List[L \/ R]): List[R] = for { \/-(r) <- xs } yield r

    def unvec(v: Vector[Action \/ Result]): Option[Execution[Result]] =
      if (v.all(_.isRight)) rights(v.toList).toNel
      else None

    unvec(current.actions).map(
      exe => current.ctxt match {
        case None => exe.right
        case Some(-\/((seqid, stepid))) => (exe, seqid, stepid).left.left
        case Some(\/-(stepid)) => (exe, stepid).right.left
      }
    )
  }
}

/**
 * Flag to indicate whether the global execution is `Running` or `Waiting`.
 */
sealed trait Status

object Status {
  case object Running extends Status
  case object Waiting extends Status
}

/**
  * A list of Sequences. The `Queue` could be empty of Sequences when waiting
  * for the addition of new ones.
  */
case class Queue[A](sequences: List[Sequence[A]])

object Queue {

  type Execution3[A] = (Execution[A], String, Int) \/ (Execution[A], Int) \/ Execution[A]

  def sequences[A]: Queue[A] @> List[Sequence[A]] =
    Lens.lensu((q, ss) => q.copy(sequences = ss), _.sequences)

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
                NonEmptyList(exe)
              )
            )
          ) :: q.sequences
        )
      // New Step
      case -\/(\/-((exe, i))) => head.mod(Sequence.cons((exe, i).left), q)
      // Modify current Step
      case \/-(exe) => head.mod(Sequence.cons(exe.right)((_: Sequence[A])), q)
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
    * `uncons`ing on an empty `Queue` returns an empty `Queue`.
    */
  def uncons[A](q: Queue[A]): Option[(Execution3[A], Queue[A])] =
    // Queue empty?
    q.sequences.headOption.map(seq0 =>
      Sequence.uncons(seq0) match {
        // Current Step completed
        case -\/(((exe, stepid), mseq)) => {
          mseq match {
            // No more Steps in current Sequence, remove Sequence.
            // TODO: listTailPLens?
            case None => ((exe, seq0.id, stepid).left.left, Queue(q.sequences.tailOption.getOrElse(List())))
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
}

/**
  * A list of Steps grouped by target and instrument.
  */
case class Sequence[A](id: String, steps: NonEmptyList[Step[A]])

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
      case -\/((exe, sid)) => Sequence(seq.id, Step(sid, NonEmptyList(exe)) <:: seq.steps)
      // Add Execution to current Step
      case \/-(exe) => head.mod(Step.cons(exe)(_), seq)
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
  def uncons[A](seq: Sequence[A]): (((Execution[A], Int), Option[Sequence[A]]) \/ (Execution[A], Sequence[A])) = {
    val (exe, mstep) = Step.uncons(seq.steps.head)
    mstep match {
      // No more Executions in Step, remove Step
      case None => ((exe, seq.steps.head.id), seq.steps.tail.toNel.map(Sequence(seq.id, _))).left
      // More Executions in Step, remove Execution in Step
      case Some(st) => (exe, head.set(seq, st)).right
    }
  }
}

/**
  * A list of `Executions` grouped by obersvation.
  */
case class Step[A](id: Int, executions: NonEmptyList[Execution[A]])

object Step {

  def executions[A]: Step[A] @> NonEmptyList[Execution[A]] =
    Lens.lensu((s, exes) => s.copy(executions = exes), _.executions)

  def head[A]: Step[A] @> Execution[A] = executions >=> Lens.nelHeadLens

  /**
    * Adds a `Execution` to the font of a `Step`.
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
}

/**
  * This structure holds the `Execution` currently under execution. It carries
  * information about which `Action`s have been completed and the position in
  * the `Queue` for proper insertion into the completed `Queue` when all the
  * `Execution`s are done.
  */
case class Current(actions: Vector[Action \/ Result],
                   // TODO: The following tuples should be replaced by either
                   // Sequence or Step parameters
                   ctxt: Option[(String, Int) \/ Int])

object Current {
  /**
    * Set the `Result` for the given `Action` index in `Current`.
    *
    * If the index doesn't exist, `Current` is returned unmodified.
    */
  def mark(i: Int)(r: Result)(c: Current): Current =
    Current(PLens.vectorNthPLens(i).setOr(c.actions, r.right, c.actions), c.ctxt)
}

/**
  * The result of an `Action`.
  */

sealed trait Result

object Result {
  case object OK extends Result
  case object Error extends Result
}
