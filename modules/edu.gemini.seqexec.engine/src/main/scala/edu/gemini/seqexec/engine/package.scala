// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec

import cats._
import cats.data.Kleisli
import cats.effect.IO
import edu.gemini.seqexec.engine.Result.{Error, PartialVal, PauseContext, RetVal}
import edu.gemini.seqexec.model.ActionType
import edu.gemini.seqexec.model.Model.{Conditions, Observer, Operator}

package engine {

  final case class ActionMetadata(conditions: Conditions, operator: Option[Operator], observer: Option[Observer])
  object ActionMetadata {
    val default: ActionMetadata = ActionMetadata(Conditions.default, None, None)
  }

  // This trait describe the kind of types that can fill the ActionMetadata used to generate an Action body
  trait ActionMetadataGenerator[T] {
    /*
     * generate is used to fill the ActionMetadata structure. It is expected in the future that there will be pieces of
     * data at the Sequence level, or even the Step level, all of ActionMetadataGenerator kind, that could be used
     * sequentially to produce the final ActionMetadata
     */
    def generate(a: T)(v: ActionMetadata): ActionMetadata
  }

  final case class Action(
    kind: ActionType,
    gen: ActionGen,
    state: Action.State
  )
  object Action {

    final case class State(runState: ActionState, partials: List[PartialVal])

    sealed trait ActionState

    case object Idle extends ActionState
    case object Started extends ActionState
    final case class Paused[C <: PauseContext](ctx: C) extends ActionState
    final case class Completed[V <: RetVal](r: V) extends ActionState
    final case class Failed(e: Error) extends ActionState

    object ActionState {
      // TODO: This is not always valid
      implicit val equal: Eq[ActionState] = Eq.fromUniversalEquals
//      implicit val equal: Eq[ActionState] =
//        Eq[Option[Option[Either[PauseContext, Either[RetVal, Error]]]]].contramap {
//          case Idle => None
//          case Started => Some(None)
//          case Paused(ctx) => Some(Option(Either.left(ctx)))
//          case Completed(r) => Some(Option(Right(Left(r: RetVal))))
//          case Failed(e) => Some(Option(Right(Right(e))))
//        }
    }

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
  def fromIO(kind: ActionType, t: IO[Result]): Action = Action(kind, Kleisli[IO, ActionMetadata, Result](_ => t), Action.State(Action.Idle, Nil))

  // Top level synonyms

  /**
    * This represents an actual real-world action to be done in the underlying
    * systems.
    */
  def fromTask(kind: ActionType, t: IO[Result]): Action = Action(kind, Kleisli[IO, ActionMetadata, Result](_ => t), Action.State(Action.Idle, Nil))
  /**
    * An `Execution` is a group of `Action`s that need to be run in parallel
    * without interruption. A *sequential* `Execution` can be represented with
    * an `Execution` with a single `Action`.
    */
  type Actions = List[Action]

  type ActionGen = Kleisli[IO, ActionMetadata, Result]

  type Results = List[Result]

  type FileId = String


}
