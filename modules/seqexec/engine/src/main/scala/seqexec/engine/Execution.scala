// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.Eq
import cats.implicits._
import monocle.function.Index.{index, listIndex}
import monocle.syntax.apply._
import mouse.boolean._
import seqexec.engine.Action.ActionState

/**
  * This structure holds the current `Execution` under execution. It carries
  * information about which `Action`s have been completed.
  *
  */
final case class Execution[F[_]](execution: List[Action[F]]) {

  import Execution._

  val isEmpty: Boolean = execution.isEmpty

  val actions: List[Action[F]] = execution.filter(_.state.runState.isIdle)

  val results: List[Action[F]] = execution.filter(Action.finished)

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
  val uncurrentify: Option[Actions[F]] =
    (execution.nonEmpty && finished(this)).option(results)

  /**
    * Set the `Result` for the given `Action` index in `Current`.
    *
    * If the index doesn't exist, `Current` is returned unmodified.
    */
  def mark(i: Int)(r: Result[F]): Execution[F] =
    Execution((execution &|-? index(i)).modify(a => a.copy(state = actionStateFromResult(r)(a.state))))

  def start(i: Int): Execution[F] =
    Execution((execution &|-? index(i)).modify(a => a.copy(state = a.state.copy(runState = ActionState.Started))))
}

object Execution {

  def empty[F[_]]: Execution[F] = Execution[F](Nil)

  /**
    * Make an `Execution` `Current` only if all the `Action`s in the execution
    * are pending.
    */
  def currentify[F[_]](as: Actions[F]): Option[Execution[F]] =
    (as.nonEmpty && as.forall(_.state.runState.isIdle)).option(Execution(as))

  def errored[F[_]](ex: Execution[F]): Boolean = ex.execution.exists(_.state.runState match {
    case ActionState.Failed(_) => true
    case _                => false
  })

  def finished[F[_]](ex: Execution[F]): Boolean = ex.execution.forall(_.state.runState match {
    case ActionState.Completed(_) => true
    case ActionState.Failed(_)    => true
    case _                   => false
  })

  def progressRatio[F[_]](ex: Execution[F]): (Int, Int) = (ex.results.length, ex.execution.length)

  def actionStateFromResult[F[_]](r: Result[F]): (Action.State[F] => Action.State[F]) =
    s => r match {
      case Result.OK(x)         => s.copy(runState = ActionState.Completed(x))
      case Result.OKStopped(x)  => s.copy(runState = ActionState.Completed(x))
      case Result.Partial(x)    => s.copy(partials = x :: s.partials)
      case e@Result.Error(_)    => s.copy(runState = ActionState.Failed(e))
      case c: Result.Paused[F]  => s.copy(runState = ActionState.Paused(c.ctx))
    }
}

/**
  * The result of an `Action`.
  */
sealed trait Result[+F[_]] extends Product with Serializable {
  val errMsg: Option[String] = None
}

object Result {

  // Base traits for results. They make harder to pass the wrong value.
  trait RetVal
  trait PartialVal
  trait PauseContext[F[_]]

  final case class OK[R <: RetVal](response: R) extends Result[Nothing]
  final case class OKStopped[R <: RetVal](response: R) extends Result[Nothing]
  final case class Partial[R <: PartialVal](response: R) extends Result[Nothing]
  final case class Paused[F[_]](ctx: PauseContext[F]) extends Result[F]
  // TODO: Replace the message by a richer Error type like `SeqexecFailure`
  final case class Error(msg: String) extends Result[Nothing] {
    override val errMsg: Option[String] = msg.some
  }
  object Error {
    implicit val eq: Eq[Error] = Eq.fromUniversalEquals
  }

}
