// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import java.time.Instant

import cats.implicits._
import fs2.Stream
import seqexec.model.ClientId
import seqexec.model.Observation
import seqexec.model.StepId
import seqexec.model.UserDetails

/**
  * Events generated by the user.
  */
sealed trait UserEvent[+F[_], S, U] extends Product with Serializable {
  def user: Option[UserDetails]
  def username: String = user.foldMap(_.username)
}

object UserEvent {

  final case class Start[S, U](id: Observation.Id, user: Option[UserDetails], clientId: ClientId) extends UserEvent[Nothing, S, U]
  final case class Pause[S, U](id: Observation.Id, user: Option[UserDetails]) extends UserEvent[Nothing, S, U]
  final case class CancelPause[S, U](id: Observation.Id, user: Option[UserDetails]) extends UserEvent[Nothing, S, U]
  final case class Breakpoint[S, U](id: Observation.Id, user: Option[UserDetails], step: StepId, v: Boolean) extends UserEvent[Nothing, S, U]
  final case class SkipMark[S, U](id: Observation.Id, user: Option[UserDetails], step: StepId, v: Boolean) extends UserEvent[Nothing, S, U]
  final case class Poll[S, U](clientId: ClientId) extends UserEvent[Nothing, S, U] {
    val user: Option[UserDetails] = None
  }
  // Generic event to put a function in the main Stream process, which takes an
  // action depending on the current state
  final case class GetState[F[_], S, U](f: S => Option[Stream[F, Event[F, S, U]]]) extends UserEvent[F, S, U] {
    val user: Option[UserDetails] = None
  }
  // Generic event to put a function in the main Process process, which changes the state
  // depending on the current state
  final case class ModifyState[F[_], S, U](f: Handle[F, S, Event[F, S, U], U]) extends UserEvent[F, S, U] {
    val user: Option[UserDetails] = None
  }
  // Calls a user given function in the main Stream process to stop an Action.
  // It sets the Sequence to be stopped. The user function is called only if the Sequence is running.
  final case class ActionStop[F[_], S, U](id: Observation.Id, f: S => Option[Stream[F, Event[F, S, U]]]) extends UserEvent[F, S, U] {
    val user: Option[UserDetails] = None
  }

  // Uses `cont` to resume execution of a paused Action. If the Action is not paused, it does nothing.
  final case class ActionResume[F[_], S, U](id: Observation.Id, i: Int, cont: Stream[F,
    Result[F]]) extends UserEvent[F, S, U] {
    val user: Option[UserDetails] = None
  }

  final case class LogDebug[S, U](msg: String, timestamp: Instant) extends UserEvent[Nothing, S, U] {
    val user: Option[UserDetails] = None
  }

  final case class LogInfo[S, U](msg: String, timestamp: Instant) extends UserEvent[Nothing, S, U] {
    val user: Option[UserDetails] = None
  }

  final case class LogWarning[S, U](msg: String, timestamp: Instant) extends UserEvent[Nothing, S, U] {
    val user: Option[UserDetails] = None
  }

  final case class LogError[S, U](msg: String, timestamp: Instant) extends UserEvent[Nothing, S, U] {
    val user: Option[UserDetails] = None
  }
}
