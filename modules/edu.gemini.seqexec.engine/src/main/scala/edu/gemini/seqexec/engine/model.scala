package edu.gemini.seqexec.engine

import scala.collection.immutable.IntMap

import scalaz._
import Scalaz._

/**
 * Input `Sequence` and `Status` of the execution. This is the top level state
 * data type to be used by the `Engine`. This is what is passed as output to
 * clients.
 */
case class QueueStatus(queue: Queue, status: Status)

object QueueStatus {

  val queue: QueueStatus @> Queue =
    Lens.lensu((ss, q1) => ss.copy(queue = q1), _.queue)

  val status: QueueStatus @> Status =
    Lens.lensu((ss, st1) => ss.copy(status = st1), _.status)

  /**
    * Lens for the remaining Queue.
    */
  val pending: QueueStatus @> Queue.Pending = queue >=> Queue.pending

  /**
    * Lens for the current Execution.
    */
  val current: QueueStatus @> Queue.Current = queue >=> Queue.current

  /**
    * Lens for the completed Queue.
    */
  val done: QueueStatus @> Queue.Done = queue >=> Queue.done

  /**
    * Promote the next pending `Execution` to current when the current
    * `Execution` is empty. If the current `Execution` is returns `None`.
    */
  def prime(ss0: QueueStatus): Option[QueueStatus] = {

    // Read next pending Execution.
    def peek(qs: QueueStatus): Execution.Pending =
      // Sequence
      (qs.queue.pending.headOption >>=
        // Step
        (_.headOption) >>=
        // Execution
        (_.headOption)
      ).getOrElse(List())

    // Remove next pending Execution.
    def remove(qs: QueueStatus): QueueStatus =
      (QueueStatus.pending.partial >=>
         PLens.listHeadPLens[Sequence.Pending] >=>
           PLens.listHeadPLens[Step.Pending] >=>
         PLens.listHeadPLens[Execution.Pending]
      ).mod(_.tailOption.getOrElse(List()), qs)

    def toIntMap[A](l: List[A]): IntMap[A] =
      IntMap(l.zipWithIndex.map(s => (s._2, s._1)).toSeq: _*)

    if (QueueStatus.current.get(ss0).isEmpty) {
      // Copy next pending execution to current execution
      val ss1 = QueueStatus.current.set(ss0, toIntMap(peek(ss0)))
      // `QueueStatus` with next pending execution removed
      Some(remove(ss1))
    } else { None }
  }


  /*
   * Given the index of a completed `Action` in the current Execution, it moves
   * such Action to the completed actions of the next done `Execution`. If the
   * current Step is empty, it does nothing.
   */
  def shift(i: Int)(ss0: QueueStatus): QueueStatus = {

    // Add index to the next done execution.
    def add(steps: Queue.Done): Queue.Done =
      (PLens.listHeadPLens[Sequence.Done] >=>
       PLens.listHeadPLens[Step.Done] >=>
       PLens.listHeadPLens[Execution.Done]
      ).mod(i :: _, steps)

    // Remove action from current execution
    val ss1 = QueueStatus.current.mod(_ - i, ss0)
    // Add action to completed
    QueueStatus.done.mod(add, ss1)
  }
}

/**
  * A triplet with the `Sequence`s waiting for execution, the current
  * `Execution` and the completed `Sequence`s.
  *
  * This is what gets loaded every night for observation.
  */
case class Queue(
  pending: Queue.Pending,
  current: Queue.Current,
  done: Queue.Done
)

object Queue {

  type Pending = List[Sequence.Pending]

  type Current = Sequence.Current

  type Done = List[Sequence.Done]

  val pending: Queue @> Queue.Pending =
    Lens.lensu((q, qp) => q.copy(pending = qp), _.pending)

  val current: Queue @> Queue.Current =
    Lens.lensu((q, qc) => q.copy(current = qc), _.current)

  val done: Queue @> Queue.Done =
    Lens.lensu((q, qd) => q.copy(done = qd), _.done)
}

/**
 * Global flag to indicate execution status.
 */
sealed trait Status

object Status {
  case object Running extends Status
  case object Waiting extends Status
}

/**
  * A triplet of remaining `Step`s, current `Execution` and completed `Step`s.
  * The `Step`s in a `Sequence` are roughly grouped by target and instrument.
  */
case class Sequence(
  pending: Sequence.Pending,
  current: Sequence.Current,
  done: Sequence.Done
)

object Sequence {

  type Pending = List[Step.Pending]

  type Current = Step.Current

  type Done = List[Step.Done]

  val pending: Sequence @> Sequence.Pending =
    Lens.lensu((s, ns) => s.copy(pending = ns), _.pending)

  val current: Sequence @> Sequence.Current =
    Lens.lensu((s, ns) => s.copy(current = ns), _.current)

  val done: Sequence @> Sequence.Done =
    Lens.lensu((s, ns) => s.copy(done = ns), _.done)
}

/**
  * A triplet of remaining `Execution`s, current `Execution` and completed
  * `Execution`s. These `Execution` are grouped by observation in common.
  */

case class Step(
  pending: Step.Pending,
  current: Step.Current,
  done: Step.Done
)

object Step {

  type Pending = List[Execution.Pending]

  type Current = Execution.Current

  type Done = List[Execution.Done]

  val pending: Step @> Step.Pending =
    Lens.lensu((s, ns) => s.copy(pending = ns), _.pending)

  val current: Step @> Step.Current =
    Lens.lensu((s, ns) => s.copy(current = ns), _.current)

  val done: Step @> Step.Done =
    Lens.lensu((s, ns) => s.copy(done = ns), _.done)
}

/**
  * A triplet of remaining `Action`s, current `Execution` being executed and
  * completed `Action`s. The `Actions`s of an `Execution` are executed
  * in parallel and atomically, meaning that they can't be interrupted while any
  * action is still being executed.
  *
  * A sequential `Action` can be represented with just one pending `Action`.
  */

object Execution {

  type Pending = List[Action]

  /**
    * Actions with static indexing. This is meant to be used for the transition
    * of parallel actions from ongoing to completed. An ordinary list won't keep
    * the original index as actions are removed.
    */
  type Current = IntMap[Action]

  /**
   * A list of successfully completed actions represented with an index. This
   * index can be used to backtrack the correspondent original action.
   */
  type Done = List[Int]
}

/**
  * The result of an `Action`.
  */
sealed trait Result

object Result {
  case object OK extends Result
  case object Error extends Result
}
