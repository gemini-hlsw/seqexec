// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.Eq
import seqexec.model.Model.Resource
import monocle.function.Index.{index, listIndex}
import monocle.syntax.apply._
import mouse.boolean._

/**
  * This structure holds the current `Execution` under execution. It carries
  * information about which `Action`s have been completed.
  *
  */
final case class Execution(execution: List[Action]) {

  import Execution._

  val isEmpty: Boolean = execution.isEmpty

  val actions: List[Action] = execution.filter(_.state.runState.isIdle)

  val results: List[Action] = execution.filter(Action.finished)

  /**
    * Calculate `Execution` `Status` based on the underlying `Action`s.
    *
    */
  def status: Status =
    if (execution.forall(_.state.runState.isIdle)) Status.Waiting
    // Empty execution is handled here
    else if (finished(this)) Status.Completed
    else if (isEmpty) Status.Completed
    else Status.Running

  /**
    * Obtain the resulting `Execution` only if all actions have been completed.
    *
    */
  val uncurrentify: Option[Actions] =
    (execution.nonEmpty && finished(this)).option(results)

  /**
    * Set the `Result` for the given `Action` index in `Current`.
    *
    * If the index doesn't exist, `Current` is returned unmodified.
    */
  def mark(i: Int)(r: Result): Execution =
    Execution((execution &|-? index(i)).modify(a => a.copy(state = actionStateFromResult(r)(a.state))))

  def start(i: Int): Execution =
    Execution((execution &|-? index(i)).modify(a => a.copy(state = a.state.copy(runState = Action.Started))))
}

object Execution {

  val empty: Execution = Execution(Nil)

  /**
    * Make an `Execution` `Current` only if all the `Action`s in the execution
    * are pending.
    */
  def currentify(as: Actions): Option[Execution] =
    (as.nonEmpty && as.forall(_.state.runState.isIdle)).option(Execution(as))

  def errored(ex: Execution): Boolean = ex.execution.exists(_.state.runState match {
    case Action.Failed(_) => true
    case _                => false
  })

  def finished(ex: Execution): Boolean = ex.execution.forall(_.state.runState match {
    case Action.Completed(_) => true
    case Action.Failed(_)    => true
    case _                   => false
  })

  def progressRatio(ex: Execution): (Int, Int) = (ex.results.length, ex.execution.length)

  def actionStateFromResult(r: Result): (Action.State => Action.State) = s => r match {
    case Result.OK(x)         => s.copy(runState = Action.Completed(x))
    case Result.Partial(x, _) => s.copy(partials = x :: s.partials )
    case Result.Paused(c)     => s.copy(runState = Action.Paused(c))
    case e@Result.Error(_)    => s.copy(runState = Action.Failed(e))
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
  trait PauseContext

  final case class OK[R <: RetVal](response: R) extends Result
  final case class Partial[R <: PartialVal](response: R, continuation: ActionGen) extends Result
  final case class Paused[C <: PauseContext](ctx: PauseContext) extends Result
  // TODO: Replace the message by a richer Error type like `SeqexecFailure`
  final case class Error(msg: String) extends Result {
    override val errMsg: Option[String] = Some(msg)
  }
  object Error {
    implicit val eq: Eq[Error] = Eq.fromUniversalEquals
  }

  sealed trait Response extends RetVal
  final case class Configured(resource: Resource) extends Response
  final case class Observed(fileId: FileId) extends Response
  object Ignored extends Response

  final case class FileIdAllocated(fileId: FileId) extends PartialVal
}
