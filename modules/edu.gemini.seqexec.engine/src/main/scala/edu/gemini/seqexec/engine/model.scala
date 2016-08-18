package edu.gemini.seqexec.engine

import scala.collection.immutable.IntMap

import scalaz._
import scalaz.Scalaz._
import scalaz.concurrent.Task

/**
 * Input `Sequence` and `Status` of the execution. This is the fundamental
 * unit of state for the seqexec. This is what is passed as output to
 * clients.
 */
case class QueueStatus(queue: Queue, status: Status)

object QueueStatus {

  /**
   * Lens to manipulate the `Sequence` under `SeqStatus`
   */
  val queue: QueueStatus @> Queue =
    Lens.lensu((ss, q1) => ss.copy(queue = q1), _.queue)

  /**
   * Lens to manipulate the `Status` under `SeqStatus`
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
  val current: QueueStatus @> Execution.Current = queue >=> Queue.current

  /**
    * Lens for completed Steps.
    */
  val done: QueueStatus @> Queue.Done = queue >=> Queue.done

  /**
   *
   *
   */
  def prime(ss0: QueueStatus): Option[QueueStatus] = {

    def remove(l: Queue.Pending): Queue.Pending = tailOption(l).getOrElse(List())

    def toIntMap[A](l: List[A]): IntMap[A] =
      IntMap(l.zipWithIndex.map(s => (s._2, s._1)).toSeq: _*)

    if (QueueStatus.current.get(ss0).isEmpty) {
      // Peek pending step
      val h = ss0.queue.pending.headOption.getOrElse(List())
      // Convert pending step to current step
      val ss1 = QueueStatus.current.set(ss0, toIntMap(h))
      // Remove pending step
      Some(QueueStatus.pending.mod(remove, ss1))
    } else { None }
  }

  /*
   * Given the index of a completed Action in the current Step, it moves such
   * action to the correspondent list of completed actions. If the current Step
   * is empty, it promotes the next pending Step to current Step.
   */
  def shift(i: Int)(ss0: QueueStatus): QueueStatus = {

    // TODO: Stock Lens for List?
    // Add an index to the list of completed steps.
    def add(steps: Queue.Done): Queue.Done =
      steps match {
        case Nil => List(List(i))
        // Only the head Step is considered.
        case (x :: xs) => (i :: x) :: xs
      }

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
  done: List[Execution.Done],
  current: Execution.Current,
  pending: List[Execution.Pending]
)

object Queue {

  type Pending = List[Execution.Pending]

  type Done = List[Execution.Done]

  /**
    * Lens for the remaining Steps.
    */
  val pending: Queue @> Queue.Pending =
    Lens.lensu((q, newSteps) => q.copy(pending = newSteps), _.pending)

  /**
    * Lens for current actions. These are the actions being executed in
    * parallel.
    */
  val current: Queue @> Execution.Current =
    Lens.lensu((q, newStep) => q.copy(current = newStep), _.current)

  /**
    * Lens for completed Steps.
    */
  val done: Queue @> Queue.Done =
    Lens.lensu((q, newSteps) => q.copy(done = newSteps), _.done)

}

/**
 * Execution status. Either `Running` or `Waiting`.
 */
sealed trait Status

object Status {
  case object Running extends Status
  case object Waiting extends Status
}

object Execution {
  /**
   *  A list of actions to be run in parallel.
   */
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

  /**
    * This represents something to be done in the underlying systems.
    */
  type Action = Task[Result]
}

/**
  * The result of an action.
  */
sealed trait Result

object Result {
  case object OK extends Result
  case object Error extends Result
}
