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
  case class SeqStatus(seq: Sequence, st: Status)

  /**
    * Lens to manipulate the `Sequence` under `SeqStatus`
    */
  val sequence = Lens.lensu[SeqStatus, Sequence](
    (ss, seq1) => ss.copy(seq = seq1),
    _.seq
  )

  /**
    * Lens to manipulate the `Status` under `SeqStatus`
    */
  val status = Lens.lensu[SeqStatus, Status](
    (ss, st1) => ss.copy(st = st1),
    _.st
    )

  /**
    * Execution status. Either `Running` or `Waiting`.
    */
  sealed trait Status
  case object Running extends Status
  case object Waiting extends Status

  /**
    * A List of `Step`s meant to be run sequentially.
    */
  case class Sequence(stepsDone: List[StepDone], stepsCurrent: IntMap[Action], stepsPending: List[Step])

  /**
    * Lens for completed Steps.
    */
  val done = Lens.lensu[Sequence, List[StepDone]](
    (seq, newSteps) => seq.copy(stepsDone = newSteps),
    _.stepsDone
  )

  /**
    * Lens for current actions. These are the actions being executed in
    * parallel.
    */
  val current = Lens.lensu[Sequence, StepCurrent](
    (seq, newStep) => seq.copy(stepsCurrent = newStep), _.stepsCurrent
  )

  /**
    * Lens for the remaining Steps.
    */
  val pending = Lens.lensu[Sequence, List[Step]](
    (seq, newSteps) => seq.copy(stepsPending = newSteps),
    _.stepsPending
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
    * Given the index of a completed Action in the current Step, it moves such
    * action to the correspondent list of completed actions. If the current Step
    * is empty, it promotes the next pending Step to current Step.
    */
  def shift(i: Int)(ss0: SeqStatus): SeqStatus = {

    // TODO: Stock Lens for List?
    // Add an index to the list of completed steps.
    def add(steps: List[StepDone]): List[StepDone] = steps match {
      case Nil => List(List(i))
      // Only the head Step is considered.
      case (x :: xs) => (i :: x) :: xs
    }

    // TODO: Stock Lens for List?
    // Remove a Step from the list of pending Steps.
    def remove(l: List[Step]): List[Step] = tailOption(l).getOrElse(List())

    // TODO: Isn't there an easier way?
    // Convert a List to a IntMap.
    def toIntMap[A](l: List[A]): IntMap[A] =
      IntMap(l.zipWithIndex.map(s => (s._2, s._1)).toSeq: _*)

    // TODO: Lens in State monad with zoom to clean this up.
    val ss1 =
      // when the current Step is empty promote the next pending step.
      if (sequence.andThen(current).get(ss0).isEmpty) {
        // Peek pending step
        val h = sequence.andThen(pending).get(ss0).headOption.getOrElse(List())
        // Convert pending step to current step
        val ss2 = sequence.andThen(current).set(ss0, toIntMap(h))
        // Remove pending step
        sequence.andThen(pending).mod(remove, ss2)
      } else { ss0 }

    // Remove action from current step
    val ss3 = sequence.andThen(current).mod(_ - i, ss1)
    // Add it to completed
    sequence.andThen(done).mod(add, ss3)
  }
}
