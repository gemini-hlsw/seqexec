// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec

import edu.gemini.seqexec.engine.Result.{Error, PartialVal, PauseContext, RetVal}
import edu.gemini.seqexec.model.Model.{Conditions, Observer, Operator}
import edu.gemini.seqexec.model.ActionType

import scalaz.Monad
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process

package engine {

  /*
   * HandleP is a Process which has as a side effect a State machine inside a Task, which can produce other
   * Processes as output.
   * Its type parameters are:
   * A: Type of the output (usually Unit)
   * D: Type of the user data included in the state machine state.
   */
  final case class HandleP[A, D](run: Handle[(A, Option[Process[Task, Event[D]]]), D])
  object HandleP {
    def fromProcess[D](p: Process[Task, Event[D]]): HandleP[Unit, D] = HandleP[Unit, D](Applicative[({type L[T] = Handle[T, D]})#L].pure[(Unit, Option[Process[Task, Event[D]]])](((), Some(p))))
  }
  final case class ActionMetadata(conditions: Conditions, operator: Option[Operator], observer: Option[Observer])
  object ActionMetadata {
    val default: ActionMetadata = ActionMetadata(Conditions.default, None, None)
  }

  final case class Action(
    kind: ActionType,
    gen: ActionGen,
    state: Action.State
  )
  object Action {

    final case class State(runState: ActionState, partials: List[PartialVal])

    sealed trait ActionState

    object ActionState {
      implicit val equal: Equal[ActionState] = Equal.equalA
    }

    object Idle extends ActionState

    object Started extends ActionState

    final case class Paused[C <: PauseContext](ctx: C) extends ActionState

    final case class Completed[V <: RetVal](r: V) extends ActionState

    final case class Failed(e: Error) extends ActionState

    def errored(ar: Action): Boolean = ar.state.runState match {
      case Action.Failed(_) => true
      case _                => false
    }

    def finished(ar: Action): Boolean = ar.state.runState match {
      case Action.Failed(_)    => true
      case Action.Completed(_) => true
      case _                   => false
    }

    def completed(ar: Action): Boolean = ar.state.runState match {
      case Action.Completed(_) => true
      case _                   => false
    }

    def paused(ar: Action): Boolean = ar.state.runState match {
      case Action.Paused(_) => true
      case _                => false
    }
  }

}

package object engine {

  // Top level synonyms

  /**
    * This represents an actual real-world action to be done in the underlying
    * systems.
    */
  def fromTask(kind: ActionType, t: Task[Result]): Action = Action(kind, Kleisli[Task, ActionMetadata, Result](_ => t), Action.State(Action.Idle, Nil))
  /**
    * An `Execution` is a group of `Action`s that need to be run in parallel
    * without interruption. A *sequential* `Execution` can be represented with
    * an `Execution` with a single `Action`.
    */
  type Actions = List[Action]

  type ActionGen = Kleisli[Task, ActionMetadata, Result]

  type Results = List[Result]

  type FileId = String

  // Handle proper

  /**
    * Type constructor where all Seqexec side effect are managed.
    *
    * It's named `Handle` after `fs2.Handle` in order to give a hint in a future
    * migration.
    */
  type Handle[A, D] = HandleStateT[Task, A, D]
  // Helper alias to facilitate lifting.
  type HandleStateT[M[_], A, D] = StateT[M, Engine.State[D], A]

  implicit def handlePInstances[D]: Applicative[({type L[T] = HandleP[T, D]})#L] with Monad[({type L[T] = HandleP[T, D]})#L] = new Applicative[({type L[T] = HandleP[T, D]})#L] with Monad[({type L[T] = HandleP[T, D]})#L] {
    private def concatOpP(op1: Option[Process[Task, Event[D]]],
                          op2: Option[Process[Task, Event[D]]]): Option[Process[Task, Event[D]]] = (op1, op2) match {
      case (None, None)         => None
      case (Some(p1), None)     => Some(p1)
      case (None, Some(p2))     => Some(p2)
      case (Some(p1), Some(p2)) => Some(p1 ++ p2)
    }

    override def point[A](a: => A): HandleP[A, D] = HandleP(Applicative[({type L[T] = Handle[T, D]})#L].pure((a, None)))


    // I tried to use a for comprehension here, but the compiler failed with error
    // "value filter is not a member of edu.gemini.seqexec.engine.Handle"
    override def ap[A, B](fa: => HandleP[A, D])(f: => HandleP[(A) => B, D]): HandleP[B, D] = HandleP(
      f.run.flatMap{
        case (g, op2) => fa.run.map {
          case (a, op1) => (g(a), concatOpP(op1, op2)) } })

    override def bind[A, B](fa: HandleP[A, D])(f: (A) => HandleP[B, D]): HandleP[B, D] = HandleP(
      fa.run.flatMap{
        case (a, op1) => f(a).run.map{
          case (b, op2) => (b, concatOpP(op1, op2))
        }
      }
    )

  }

  implicit class HandleToHandleP[A, D](self: Handle[A, D]) {
    def toHandleP: HandleP[A, D] = HandleP(self.map((_, None)))
  }

  // The `Catchable` instance of `Handle`` needs to be manually written.
  // Without it it's not possible to use `Handle` as a scalaz-stream process effects.
  implicit def engineInstance[D]: Catchable[({type L[T] = Handle[T, D]})#L] =
    new Catchable[({type L[T] = Handle[T, D]})#L] {
      def attempt[A](a: Handle[A, D]): Handle[Throwable \/ A, D] = a.flatMap(
        x => Catchable[Task].attempt(Applicative[Task].pure(x)).liftM[({ type L[M[_], T] = HandleStateT[M, T, D]})#L]
      )
      def fail[A](err: Throwable): Handle[A, D] = Catchable[Task].fail[A](err).liftM[({ type L[M[_], T] = HandleStateT[M, T, D]})#L]
    }


}
