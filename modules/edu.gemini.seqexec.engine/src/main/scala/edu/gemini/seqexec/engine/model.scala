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
  def sequences[A]: Queue[A] @> List[Sequence[A]] =
    Lens.lensu((q, ss) => q.copy(sequences = ss), _.sequences)

  def next[A]: Queue[A] @?> Sequence[A] = sequences.partial >=> PLens.listHeadPLens

  /**
    * Returns the next `Execution` and the remaining `Queue`. If the `Execution`
    * was the last one, the `Queue` becomes empty.
    *
    * The meaning for the constructor wrappers for `Execution`s:
    *  - E \/ x \/ x: When current `Sequence` completed
    *  - x \/ E \/ x: When current `Step` completed
    *  - x \/ x \/ E: When more `Execution`s remain in the current `Step`
    *
    * `uncons`ing on an empty `Queue` returns an empty `Queue`.
    */
  def uncons[A](q: Queue[A]): Option[(Execution[A] \/ Execution[A] \/ Execution[A], Queue[A])] =
    // Queue empty?
    q.sequences.headOption.map(
      Sequence.uncons(_) match {
        // Current step completed
        case (-\/(l)) => {
          val (exe, mseq) = l
          mseq match {
            // No more Steps in current Sequence, remove Sequence.
            // TODO: listTailPLens?
            case None => (exe.left.left, Queue(q.sequences.tailOption.getOrElse(List())))
            // More Steps left in current Sequence, replace next Sequence with
            // modified one.
            case Some(seq) => (exe.right.left, next.set(q, seq).getOrElse(q))
          }
        }
        // Step ongoing
        case (\/-(r)) => {
          val (exe, seq) = r
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
    * \/-(Execution, Sequence)
    *
    * Returns the next `Execution` and the remaining `Sequence` when there are
    * more `Execution`s left in the current `Step`.
    *
    * -\/(Execution, Option[Sequence])
    *
    * Returns the next `Execution` and the remaining `Sequence` when there are
    * no more `Execution`s in the current `Step`. If this was the last `Step` of
    * the current `Sequence` it becomes `None`.
    */
  def uncons[A](seq: Sequence[A]): ((Execution[A], Option[Sequence[A]]) \/ (Execution[A], Sequence[A])) = {
    val (exe, mstep) = Step.uncons(seq.steps.head)
    mstep match {
      // Last Execution in Step, remove Step
      case None => (exe, seq.steps.tail.toNel.map(Sequence(seq.id, _))).left
      // More Executions in Step, remove Execution in Step
      case Some(st) => (exe, next.set(seq, st)).right
    }
  }
}

case class Step[A](id: Int, executions: NonEmptyList[Execution[A]])

object Step {
  def executions[A]: Step[A] @> NonEmptyList[Execution[A]] =
    Lens.lensu((s, exes) => s.copy(executions = exes), _.executions)

  /**
    * Returns the next `Execution` and the remaining `Step` if there are more
    * `Execution`s left.
    */
  def uncons[A](st: Step[A]): (Execution[A], Option[Step[A]]) =
    (st.executions.head,
     st.executions.tail.toNel.map(Step(st.id, _))
    )
}

object Current {
  // Same actions if index doesn't exist
  def mark(i: Int)(r: Result)(actions: Current): Current =
    PLens.vectorNthPLens(i).setOr(actions, r.right, actions)
}

/**
  * The result of an `Action`.
  */

sealed trait Result

object Result {
  case object OK extends Result
  case object Error extends Result
}
