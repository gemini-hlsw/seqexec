package edu.gemini.seqexec.engine

import scala.collection.immutable.IntMap

import scalaz._
import scalaz.Scalaz._
import scalaz.concurrent.Task

object State {

  /**
    * Input `Sequence` and `Status` of the execution. This is the fundamental
    * unit of state for the seqexec. This is what is passed as output to
    * clients.
    */
  case class SeqStatus(sequence: Sequence, status: Status)

  /**
    * Execution status. Either `Running` or `Waiting`.
    */
  sealed trait Status
  case object Running extends Status
  case object Waiting extends Status

  /**
    * A List of `Step`s meant to be run sequentially.
    */
  case class Sequence(done: List[StepDone],
                      current: IntMap[Action],
                      pending: List[Step]
  )

  /**
    *  A list of actions to be run in parallel.
    */
  type Step = List[Action]

  /**
    * Actions with static indexing. This is meant to be used for the transition
    * of parallel actions from ongoing to completed. An ordinary list won't keep
    * the original index as actions are removed.
    */
  type StepCurrent = IntMap[Action]

  /**
    * A list of successfully completed actions represented with an index. This
    * index can be used to backtrack the correspondent original action.
    */
  type StepDone = List[Int]

  /**
    * This represents something to be done in the underlying systems.
    */
  type Action = Task[Result]

  /**
    * The result of an action.
    */
  sealed trait Result
  case object OK extends Result
  case object Error extends Result

  /**
    * Lens to manipulate the `Sequence` under `SeqStatus`
    */
  val sequenceL: SeqStatus @> Sequence =
    Lens.lensu((ss, seq1) => ss.copy(sequence = seq1), _.sequence)

  /**
    * Lens to manipulate the `Status` under `SeqStatus`
    */
  val statusL: SeqStatus @> Status =
    Lens.lensu((ss, st1) => ss.copy(status = st1), _.status)

  /**
    * Lens for completed Steps.
    */
  val doneL: Sequence @> List[StepDone] =
    Lens.lensu((seq, newSteps) => seq.copy(done = newSteps), _.done)

  /**
    * Lens for current actions. These are the actions being executed in
    * parallel.
    */
  val currentL: Sequence @> StepCurrent =
    Lens.lensu((seq, newStep) => seq.copy(current = newStep), _.current)

  /**
    * Lens for the remaining Steps.
    */
  val pendingL: Sequence @> List[Step] =
    Lens.lensu((seq, newSteps) => seq.copy(pending = newSteps), _.pending)

  /**
    *
    *
    */
  def prime(ss0: SeqStatus): Option[SeqStatus] = {

    def remove(l: List[Step]): List[Step] = tailOption(l).getOrElse(List())

    def toIntMap[A](l: List[A]): IntMap[A] =
      IntMap(l.zipWithIndex.map(s => (s._2, s._1)).toSeq: _*)

    if (sequenceL.andThen(currentL).get(ss0).isEmpty) {
      // Peek pending step
      val h = sequenceL.andThen(pendingL).get(ss0).headOption.getOrElse(List())
      // Convert pending step to current step
      val ss1 = sequenceL.andThen(currentL).set(ss0, toIntMap(h))
      // Remove pending step
      Some(sequenceL.andThen(pendingL).mod(remove, ss1))
    } else { None }
  }

  /*
   * Given the index of a completed Action in the current Step, it moves such
   * action to the correspondent list of completed actions. If the current Step
   * is empty, it promotes the next pending Step to current Step.
   */
  def shift(i: Int)(ss0: SeqStatus): SeqStatus = {

    // TODO: Stock Lens for List?
    // Add an index to the list of completed steps.
    def add(steps: List[StepDone]): List[StepDone] =
      steps match {
        case Nil => List(List(i))
        // Only the head Step is considered.
        case (x :: xs) => (i :: x) :: xs
    }

    // Remove action from current step
    val ss1 = sequenceL.andThen(currentL).mod(_ - i, ss0)
    // Add action to completed
    sequenceL.andThen(doneL).mod(add, ss1)
  }

}
