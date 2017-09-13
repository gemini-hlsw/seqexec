// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
  * This structure holds the current `Execution` under execution. It carries
  * information about which `Action`s have been completed.
  *
  */
final case class Execution(execution: List[Action \/ Result]) {

  import Execution._

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
    else if (execution.all(finished)) Status.Completed
    else if (isEmpty) Status.Completed
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

  def errored(ar: Action \/ Result): Boolean =
    ar match {
      case (-\/(_)) => false
      case (\/-(r)) => r match {
        case Result.Error(_) => true
        case _               => false
      }
    }

  def finished(ar: Action \/ Result): Boolean =
    ar match {
      case (-\/(_)) => false
      case (\/-(r)) => r match {
        case Result.Partial(_, _) => false
        case _                    => true
      }
    }
}

/**
  * The result of an `Action`.
  */

sealed trait Result {
  val errMsg: Option[String] = None
}

object Result {

  // Base traits for results. They make harder to pass the wrong value.
  trait RetVal
  trait PartialVal

  final case class OK[R<:RetVal](response: R) extends Result
  final case class Partial[R<:PartialVal](response: R, continuation: Action) extends Result
  // TODO: Replace the message by a richer Error type like `SeqexecFailure`
  final case class Error(msg: String) extends Result {
    override val errMsg: Option[String] = Some(msg)
  }

  sealed trait Response extends RetVal
  final case class Configured(r: String) extends Response
  final case class Observed(fileId: FileId) extends Response
  object Ignored extends Response

  final case class FileIdAllocated(fileId: FileId) extends PartialVal
}
