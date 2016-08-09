package edu.gemini.seqexec.engine

import scala.collection.immutable.IntMap

import scalaz._
import scalaz.Scalaz._
import scalaz.concurrent.Task

object State {

  /**
    * Input Sequence and Status clubbed together
    *
    */
  case class SeqStatus(seq: Sequence, st: Status)

  val sequence = Lens.lensu[SeqStatus, Sequence](
    (ss, seq1) => ss.copy(seq = seq1),
    _.seq
  )

  val status = Lens.lensu[SeqStatus, Status](
    (ss, st1) => ss.copy(st = st1),
    _.st
    )

  /**
    * Status of the telescope.
    *
    */
  sealed trait Status
  case object Running extends Status
  case object Waiting extends Status

  /**
    * A List of `Step`s meant to be run sequentially.
    */
  case class Sequence(stsd: List[StepDone], stsc: IntMap[Action], sts: List[Step])

  val done = Lens.lensu[Sequence, List[StepDone]](
    (seq, stsd1) => seq.copy(stsd = stsd1), _.stsd
  )

  val current = Lens.lensu[Sequence, IntMap[Action]](
    (seq, stsc) => seq.copy(stsc = stsc), _.stsc
  )

  val pending = Lens.lensu[Sequence, List[Step]](
    (seq, sts1) => seq.copy(sts = sts1), _.sts
  )

  /**
    *  A list of actions to be run in parallel
    */
  type Step = List[Action]

  type StepDone = List[Int]

  type Action = Task[Result]

  /**
    * The result of an action.
    *
    */
  sealed trait Result
  case object OK extends Result
  case object Error extends Result

  /**
    * Given an index a completed Action, move it to the list of completed
    * Actions.
    */
  def shift(i: Int)(ss0: SeqStatus): SeqStatus = {

    // TODO: Stock Lens for List?
    def add(stsd: List[StepDone]): List[StepDone] = stsd match {
      case Nil => List(List(i))
      case (x :: xs) => (i :: x) :: xs
    }

    // TODO: Stock Lens for List
    def remove(l: List[Step]): List[Step] = tailOption(l).getOrElse(List())

    // TODO: Isn't there an easier way?
    def toIntMap[A](l: List[A]): IntMap[A] =
      IntMap(l.zipWithIndex.map(s => (s._2, s._1)).toSeq: _*)

    // TODO: Lens in State monad with zoom to clean this up.
    // If there are no current Actions promote them from pending Actions.
    val ss1 =
      if (sequence.andThen(current).get(ss0).isEmpty) {
        val h = sequence.andThen(pending).get(ss0).headOption.getOrElse(List())
        val ss2 = sequence.andThen(current).set(ss0, toIntMap(h))
        sequence.andThen(pending).mod(remove, ss2)
      } else { ss0 }

    val ss3 = sequence.andThen(current).mod(_ - i, ss1)
    sequence.andThen(done).mod(add, ss3)
  }
}


