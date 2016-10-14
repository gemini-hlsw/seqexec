package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * This structure holds the current `Execution` under execution. It carries
  * information about which `Action`s have been completed.
  *
  */
case class Current(execution: Execution[Action \/ Result]) {

  val isEmpty: Boolean = execution.isEmpty

  val actions: List[Action] = {
    def lefts[L, R](xs: List[L \/ R]): List[L] = xs.collect { case -\/(l) => l }
    lefts(execution.toList)
  }

  val results: List[Result] = {
    def rights[L, R](xs: List[L \/ R]): List[R] = xs.collect { case \/-(r) => r }
    rights(execution.toList)
  }

  /**
    * Calculate `Execution` `Status` based on the underlying `Action`s.
    *
    */
  def status: Status =
    if (execution.isEmpty || execution.all(_.isLeft)) Status.Waiting
    else if (execution.all(_.isRight)) Status.Completed
    else Status.Running

  /**
    * Obtain the resulting `Execution` only if all actions have been completed.
    *
    */
  val uncurrentify: Option[Execution[Result]] =
    execution.all(_.isRight).option(results)

  /**
    * Set the `Result` for the given `Action` index in `Current`.
    *
    * If the index doesn't exist, `Current` is returned unmodified.
    */
  def mark(i: Int)(r: Result): Current =
    Current(PLens.listNthPLens(i).setOr(execution, r.right, execution))
}

object Current {

  val empty: Current = Current(Nil)

  /**
    * Make an `Execution` `Current` only if all the `Action`s in the execution
    * are pending.
    *
    */
  def currentify(exe: Execution[Action]): Option[Current] =
    (exe.nonEmpty).option(Current(exe.map(_.left)))
}

/**
  * The result of an `Action`.
  */
sealed trait Result

object Result {
  case class OK[R](r: R) extends Result
  case class Error[E](e: E) extends Result
}
