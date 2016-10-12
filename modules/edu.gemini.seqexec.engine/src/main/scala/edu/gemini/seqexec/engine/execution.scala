package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * This structure holds the `Execution` currently under execution. It carries
  * information about which `Action`s have been completed and the position in
  * the `Queue` for proper insertion into the completed `Queue` when all the
  * `Execution`s are done.
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

  def status: Status =
    if (execution.isEmpty || execution.all(_.isLeft)) Status.Waiting
    else if (execution.all(_.isRight)) Status.Completed
    else Status.Running

  val uncurrentify: Option[Execution[Result]] = execution.all(_.isRight).option(results)

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

  def currentify(exe: Execution[Action]): Option[Current] =
    (!exe.isEmpty).option(Current(exe.map(_.left)))

}

/**
  * The result of an `Action`.
  */
sealed trait Result

object Result {
  case class OK[R](r: R) extends Result
  case class Error[E](e: E) extends Result
}
