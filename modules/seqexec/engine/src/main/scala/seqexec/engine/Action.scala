// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import fs2.Stream
import seqexec.engine.Result.{Error, PartialVal, PauseContext, RetVal}
import seqexec.model.ActionType
import seqexec.model.enum.ActionStatus
import monocle.macros.Lenses

@Lenses
final case class Action[F[_]](
  kind: ActionType,
  gen: Stream[F, Result[F]],
  state: Action.State[F]
)

object Action {

  final case class State[F[_]](runState: ActionState[F], partials: List[PartialVal])

  sealed trait ActionState[+F[_]] {
    def isIdle: Boolean = false

    def errored: Boolean = this match {
      case ActionState.Failed(_) => true
      case _                     => false
    }

    def finished: Boolean = this match {
      case ActionState.Failed(_)    => true
      case ActionState.Completed(_) => true
      case _                        => false
    }

    def completed: Boolean = this match {
      case ActionState.Completed(_) => true
      case _                        => false
    }

    def paused: Boolean = this match {
      case ActionState.Paused(_) => true
      case _                     => false
    }

    def active: Boolean = this match {
      case ActionState.Paused(_) |
           ActionState.Started => true
      case _                   => false
    }

    def started: Boolean = this match {
      case ActionState.Started => true
      case _                   => false
    }

    def actionStatus: ActionStatus = ActionState.actionStateToStatus(this)

  }

  object ActionState {

    case object Idle extends ActionState[Nothing] {
      override val isIdle: Boolean = true
    }
    case object Started extends ActionState[Nothing]
    final case class Paused[F[_]](ctx: PauseContext[F]) extends ActionState[F]
    final case class Completed[V <: RetVal](r: V) extends ActionState[Nothing]
    final case class Failed(e: Error) extends ActionState[Nothing]

    private def actionStateToStatus[F[_]](s: ActionState[F]): ActionStatus =
      s match {
        case Idle         => ActionStatus.Pending
        case Completed(_) => ActionStatus.Completed
        case Started      => ActionStatus.Running
        case Failed(_)    => ActionStatus.Failed
        case _: Paused[F] => ActionStatus.Paused
      }

  }

  def errored[F[_]](ar: Action[F]): Boolean = ar.state.runState.errored

  def finished[F[_]](ar: Action[F]): Boolean = ar.state.runState.finished

  def completed[F[_]](ar: Action[F]): Boolean = ar.state.runState.completed

  def paused[F[_]](ar: Action[F]): Boolean = ar.state.runState.paused

  def active[F[_]](ar: Action[F]): Boolean = ar.state.runState.active
}
