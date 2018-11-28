// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec

import fs2.Stream
import seqexec.engine.Result.{Error, PartialVal, PauseContext, RetVal}
import seqexec.model.ActionType

package engine {

  final case class Action[F[_]](
    kind: ActionType,
    gen: Stream[F, Result],
    state: Action.State
  )
  object Action {

    final case class State(runState: ActionState, partials: List[PartialVal])

    sealed trait ActionState {
      def isIdle: Boolean = false

      def errored: Boolean = this match {
        case Action.Failed(_) => true
        case _                => false
      }

      def finished: Boolean = this match {
        case Action.Failed(_)    => true
        case Action.Completed(_) => true
        case _                   => false
      }

      def completed: Boolean = this match {
        case Action.Completed(_) => true
        case _                   => false
      }

      def paused: Boolean = this match {
        case Action.Paused(_) => true
        case _                => false
      }

      def active: Boolean = this match {
        case Action.Paused(_) |
             Action.Started     => true
        case _                  => false
      }

      def started: Boolean = this match {
        case Action.Started => true
        case _              => false
      }

    }

    case object Idle extends ActionState {
      override def isIdle: Boolean = true
    }
    case object Started extends ActionState
    final case class Paused[C <: PauseContext](ctx: C) extends ActionState
    final case class Completed[V <: RetVal](r: V) extends ActionState
    final case class Failed(e: Error) extends ActionState

    def errored[F[_]](ar: Action[F]): Boolean = ar.state.runState.errored

    def finished[F[_]](ar: Action[F]): Boolean = ar.state.runState.finished

    def completed[F[_]](ar: Action[F]): Boolean = ar.state.runState.completed

    def paused[F[_]](ar: Action[F]): Boolean = ar.state.runState.paused

    def active[F[_]](ar: Action[F]): Boolean = ar.state.runState.active
  }

}

package object engine {

  // Top level synonyms

  /**
    * This represents an actual real-world action to be done in the underlying
    * systems.
    */
  def fromF[F[_]](kind: ActionType, t: F[Result]): Action[F] = Action(kind, Stream.eval(t), Action
    .State(Action.Idle, Nil))

  /**
    * An `Execution` is a group of `Action`s that need to be run in parallel
    * without interruption. A *sequential* `Execution` can be represented with
    * an `Execution` with a single `Action`.
    */
  type Actions[F[_]] = List[Action[F]]

  type Results = List[Result]

}
