package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * This structure holds the current `Execution` under execution. It carries
  * information about which `Action`s have been completed.
  *
  */
case class Execution(execution: List[Action \/ Result]) {

  val isEmpty: Boolean = execution.isEmpty

  val actions: Actions = {
    def lefts[L, R](xs: List[L \/ R]): List[L] = xs.collect { case -\/(l) => l }
    lefts(execution)
  }

  val results: Results = {
    def rights[L, R](xs: List[L \/ R]): List[R] = xs.collect { case \/-(r) => r }
    rights(execution)
  }

  /**
    * Calculate `Execution` `Status` based on the underlying `Action`s.
    *
    */
  def status: Status =
    if (execution.forall(_.isLeft)) Status.Waiting
    // Empty execution is handled here
    else if (execution.all(_.isRight)) Status.Completed
    else Status.Running

  /**
    * Obtain the resulting `Execution` only if all actions have been completed.
    *
    */
  val uncurrentify: Option[Results] =
    (execution.nonEmpty && execution.all(_.isRight)).option(results)

  /**
    * Set the `Result` for the given `Action` index in `Current`.
    *
    * If the index doesn't exist, `Current` is returned unmodified.
    */
  def mark(i: Int)(r: Result): Execution =
    Execution(PLens.listNthPLens(i).setOr(execution, r.right, execution))
}

object Execution {

  val empty: Execution = Execution(Nil)

  /**
    * Make an `Execution` `Current` only if all the `Action`s in the execution
    * are pending.
    *
    */
  def currentify(as: Actions): Option[Execution] =
    as.nonEmpty.option(Execution(as.map(_.left)))

}

/**
  * The result of an `Action`.
  */
sealed trait Result

object Result {
  case class OK[R](r: R) extends Result
  case class Error[E](e: E) extends Result
}
