package edu.gemini.seqexec.engine

import scala.collection.immutable.IntMap

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

/**
 * Input `Sequence` and `Status` of the execution. This is the fundamental
 * unit of state for the seqexec. This is what is passed as output to
 * clients.
 */
case class QueueStatus(queue: Queue, status: Status)

object QueueStatus {

  /**
   * Lens to manipulate the `Sequence` under `Engine`
   */
  val queue: QueueStatus @> Queue =
    Lens.lensu((ss, q1) => ss.copy(queue = q1), _.queue)

  /**
   * Lens to manipulate the `Status` under `Engine`
   */
  val status: QueueStatus @> Status =
    Lens.lensu((ss, st1) => ss.copy(status = st1), _.status)

  /**
    * Lens for the remaining Steps.
    */
  val pending: QueueStatus @> Queue.Pending = queue >=> Queue.pending

  /**
    * Lens for current actions. These are the actions being executed in
    * parallel.
    */
  val current: QueueStatus @> Queue.Current = queue >=> Queue.current

  /**
    * Lens for completed Steps.
    */
  val done: QueueStatus @> Queue.Done = queue >=> Queue.done

  /**
   *
   *
   */
  def prime(ss0: QueueStatus): Option[QueueStatus] = {

    def peek(qs: QueueStatus): Execution.Pending =
      // Sequence
      (qs.queue.pending.headOption >>=
        // Step
        (_.headOption) >>=
        // Execution
        (_.headOption)
      ).getOrElse(List())

    def remove(qs: QueueStatus): QueueStatus =
      (QueueStatus.pending.partial >=>
         PLens.listHeadPLens[Sequence.Pending] >=>
           PLens.listHeadPLens[Step.Pending] >=>
         PLens.listHeadPLens[Execution.Pending]
      ).mod(_.tailOption.getOrElse(List()), qs)

    def toIntMap[A](l: List[A]): IntMap[A] =
      IntMap(l.zipWithIndex.map(s => (s._2, s._1)).toSeq: _*)

    if (QueueStatus.current.get(ss0).isEmpty) {
      // Convert pending step to current step
      val ss1 = QueueStatus.current.set(ss0, toIntMap(peek(ss0)))
      // Remove pending step
      Some (remove(ss1))
      // Some(QueueStatus.pending.mod(remove, ss1))
    } else { None }
  }


  /*
   * Given the index of a completed Action in the current Step, it moves such
   * action to the correspondent list of completed actions. If the current Step
   * is empty, it promotes the next pending Step to current Step.
   */
  def shift(i: Int)(ss0: QueueStatus): QueueStatus = {

    // Add an index to the list of completed steps.
    def add(steps: Queue.Done): Queue.Done =
      (PLens.listHeadPLens[Sequence.Done] >=>
       PLens.listHeadPLens[Step.Done] >=>
       PLens.listHeadPLens[Execution.Done]
      ).mod(i :: _, steps)

    // Remove action from current step
    val ss1 = QueueStatus.current.mod(_ - i, ss0)
    // Add action to completed
    QueueStatus.done.mod(add, ss1)
  }
}

/**
  * A List of `Step`s meant to be run sequentially.
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
 * Execution status. Either `Running` or `Waiting`.
 */
sealed trait Status

object Status {
  case object Running extends Status
  case object Waiting extends Status
}

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

object Execution {
  /**
    * This represents something to be done in the underlying systems.
    */
  type Action = Task[Result]

  /**
    * Actions with static indexing. This is meant to be used for the transition
    * of parallel actions from ongoing to completed. An ordinary list won't keep
    * the original index as actions are removed.
    */
  type Current = IntMap[Action]

  /**
   *  A list of actions to be run in parallel.
   */
  type Pending = List[Action]

  /**
   * A list of successfully completed actions represented with an index. This
   * index can be used to backtrack the correspondent original action.
   */
  type Done = List[Int]
}

/**
  * The result of an action.
  */
sealed trait Result

object Result {
  case object OK extends Result
  case object Error extends Result
}
