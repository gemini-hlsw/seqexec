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

  def prime(st: State): (Option[(Execution[Action], State)]) = {
    // XXX: Use temporal values for better readability
    // For now it's easier to read inside-out
    Queue.uncons(st.pending).map(
      t => {
        val (exe3, qp) = t
        (Queue.execution(exe3),
         State.current.set(
           State.pending.set(
             State.done.set(
               st,
               Queue.cons(unload(st.current))(st.done)
             ),
             qp),
           load(exe3)
         )
        )
      }
    )
  }

  def load(exe3: Queue.Execution3[Action]): Current = {

    def vec(exe: Execution[Action]): Vector[Action \/ Result] = exe.map(_.left).toVector

    exe3 match {
      case -\/(-\/((exe, seqid, stepid))) => Current(vec(exe), Some((seqid, stepid).left))
      case -\/(\/-((exe, stepid))) => Current(vec(exe), Some(stepid.right))
      case \/-(exe) => Current(vec(exe), None)
    }
  }

  def unload(current: Current): Queue.Execution3[Result] = {
    // XXX: Return None when not all are results!
    def unvec(v: Vector[Action \/ Result]): Execution[Result] = {
      rights(v.toList).toNel.getOrElse(???)
    }

    val exe = unvec(current.actions)

    current.ctxt match {
      case None => exe.right
      case Some(-\/((seqid, stepid))) => (exe, seqid, stepid).left.left
      case Some(\/-(stepid)) => (exe, stepid).right.left
    }
  }

  // not available in scalaz?
  private def rights[L, R](xs: List[L \/ R]): List[R] =
    for { \/-(r) <- xs } yield r
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

  def next[A]: Queue[A] @?> Sequence[A] = sequences.partial >=> PLens.listHeadPLens

  def execution[A](exe3: Execution3[A]): Execution[A] = ???

  /**
    * Adds an `Execution` to the `Queue`.
    *
    * The `Execution` wrappers mean:
    * - x \/ x \/ E: Adds it to the current `Step`.
    * - x \/ E \/ x: Creates a new `Step` ands adds it to the current `Sequence`.
    * - E \/ x \/ x: Creates a new `Sequence` and adds it to the front of the `Queue`.
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
      case -\/(\/-((exe, i))) => next.mod(Sequence.cons((exe, i).left), q)
      // Current Step
      case \/-(exe) => next.mod(Sequence.cons(exe.right)((_: Sequence[A])), q)
    }

  /**
    * Returns the next `Execution` and the remaining `Queue`. If the `Execution`
    * was the last one, the `Queue` becomes empty.
    *
    * The `Execution` wrappers mean:
    *  - x \/ x \/ E: When more `Execution`s remain in the current `Step`
    *  - x \/ E \/ x: When current `Step` completed
    *  - E \/ x \/ x: When current `Sequence` completed
    *
    * `uncons`ing on an empty `Queue` returns an empty `Queue`.
    */
  def uncons[A](q: Queue[A]): Option[(Execution3[A], Queue[A])] =
    // Queue empty?
    q.sequences.headOption.map(seq0 =>
      Sequence.uncons(seq0) match {
        // Current step completed
        case -\/(((exe, stepid), mseq)) => {
          mseq match {
            // No more Steps in current Sequence, remove Sequence.
            // TODO: listTailPLens?
            case None => ((exe, seq0.id, stepid).left.left, Queue(q.sequences.tailOption.getOrElse(List())))
            // More Steps left in current Sequence, replace next Sequence with
            // modified one.
            case Some(seq) => ((exe, stepid).right.left, next.set(q, seq).getOrElse(q))
          }
        }
        // Step ongoing
        case \/-((exe, seq)) => {
          // seq has the execution already extracted
          (exe.right, next.set(q, seq).getOrElse(q))
        }
      }
    )
}

case class Sequence[A](id: String, steps: NonEmptyList[Step[A]])

object Sequence {
  def steps[A]: Sequence[A] @> NonEmptyList[Step[A]] =
    Lens.lensu((s, sts) => s.copy(steps = sts), _.steps)

  def next[A]: Sequence[A] @> Step[A] = steps >=> Lens.nelHeadLens

  /**
    * Adds an `Execution` to the `Sequence`.
    *
    * The `Execution` wrappers mean:
    * - x \/ E: Adds it to the current `Step`.
    * - E \/ x: Creates a new `Step` ands adds it to the front of the `Sequence`.
    */
  def cons[A](exe2: (Execution[A], Int) \/ Execution[A])(seq: Sequence[A]): Sequence[A] =
    exe2 match {
      // Add new Step
      case -\/((exe, sid)) => Sequence(seq.id, Step(sid, NonEmptyList(exe)) <:: seq.steps)
      // Add to current Step
      case \/-(exe) => next.mod(Step.cons(exe)(_), seq)
    }


  /**
    * Returns the next `Execution` and the remaining of the `Sequence`.
    *
    * The wrappers mean:
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
      // Last Execution in Step, remove Step
      case None => ((exe, seq.steps.head.id), seq.steps.tail.toNel.map(Sequence(seq.id, _))).left
      // More Executions in Step, remove Execution in Step
      case Some(st) => (exe, next.set(seq, st)).right
    }
  }
}

case class Step[A](id: Int, executions: NonEmptyList[Execution[A]])

object Step {

  def executions[A]: Step[A] @> NonEmptyList[Execution[A]] =
    Lens.lensu((s, exes) => s.copy(executions = exes), _.executions)

  def next[A]: Step[A] @> Execution[A] = executions >=> Lens.nelHeadLens

  /**
    * Adds a `Execution` to the font of a `Step`.
    */
  def cons[A](exe: Execution[A])(st: Step[A]): Step[A] = Step(st.id, exe <:: st.executions)

  /**
    * Returns the next `Execution` and the remaining `Step` if there are more
    * `Execution`s left.
    */
  def uncons[A](st: Step[A]): (Execution[A], Option[Step[A]]) =
    (st.executions.head,
     st.executions.tail.toNel.map(Step(st.id, _))
    )
}

case class Current(actions: Vector[Action \/ Result],
                   ctxt: Option[(String, Int) \/ Int])

object Current {
  // Same actions if index doesn't exist
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
