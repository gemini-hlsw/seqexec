// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.implicits._
import cats.effect.Sync
import fs2.Stream
import gem.Observation
import java.time.Instant
import seqexec.model.ClientId
import seqexec.model.UserDetails
import seqexec.model.StepId
import seqexec.engine.SystemEvent._
import seqexec.engine.UserEvent._

/**
  * Anything that can go through the Event Queue.
  */
sealed trait Event[F[_], +S, +U] extends Product with Serializable

object Event {
  final case class EventUser[F[_], S, U](ue: UserEvent[F, S, U]) extends Event[F, S, U]
  final case class EventSystem[F[_]](se: SystemEvent[F]) extends Event[F, Nothing, Nothing]

  def start[F[_], S, U](id: Observation.Id, user: UserDetails, clientId: ClientId, userCheck: S => Boolean): Event[F, S, U] =
    EventUser[F, S, U](Start[S, U](id, user.some, clientId, userCheck))
  def pause[F[_], S, U](id: Observation.Id, user: UserDetails): Event[F, S, U] = EventUser[F, S, U](Pause(id, user.some))
  def cancelPause[F[_], S, U](id: Observation.Id, user: UserDetails): Event[F, S, U] = EventUser[F, S, U](CancelPause(id, user.some))
  def breakpoint[F[_], S, U](id: Observation.Id, user: UserDetails, step: StepId, v: Boolean): Event[F, S, U] = EventUser[F, S, U](Breakpoint(id, user.some, step, v))
  def skip[F[_], S, U](id: Observation.Id, user: UserDetails, step: StepId, v: Boolean): Event[F, S, U] = EventUser[F, S, U](SkipMark(id, user.some, step, v))
  def poll[F[_]](clientId: ClientId): Event[F, Nothing, Nothing] = EventUser[F, Nothing, Nothing](Poll(clientId))
  def getState[F[_], S, U](f: S => Option[Stream[F, Event[F, S, U]]]): Event[F, S, U] = EventUser[F, S, U](GetState(f))
  def modifyState[F[_], S, U](f: Handle[F, S, Event[F, S, U], U]): Event[F, S, U] = EventUser[F, S, U](ModifyState(f))
  def actionStop[F[_], S, U](id: Observation.Id, f: S => Option[Stream[F, Event[F, S, U]]]): Event[F, S, U] = EventUser[F, S, U](ActionStop(id, f))
  def actionResume[F[_], S, U](id: Observation.Id, i: Int, c: Stream[F, Result[F]]): Event[F, S, U] =
    EventUser[F, S, U](ActionResume(id, i, c))
  def logDebugMsg[F[_], S, U](msg: String, ts: Instant): Event[F, S, U] = EventUser[F, S, U](LogDebug(msg, ts))
  def logDebugMsgF[F[_]: Sync, S, U](msg: String): F[Event[F, S, U]] = Sync[F].delay(Instant.now).map(t => EventUser[F, S, U](LogDebug(msg, t)))
  def logInfoMsg[F[_], S, U](msg: String, ts: Instant): Event[F, S, U] = EventUser[F, S, U](LogInfo(msg, ts))
  def logWarningMsg[F[_], S, U](msg: String, ts: Instant): Event[F, S, U] = EventUser[F, S, U](LogWarning(msg, ts))
  def logErrorMsg[F[_], S, U](msg: String, ts: Instant): Event[F, S, U] = EventUser[F, S, U](LogError(msg, ts))
  def logErrorMsgF[F[_]: Sync, S, U](msg: String): F[Event[F, S, U]] = Sync[F].delay(Instant.now).map(t => EventUser[F, S, U](LogError(msg, t)))

  def failed[F[_]](id: Observation.Id, i: Int, e: Result.Error): Event[F, Nothing, Nothing] = EventSystem[F](Failed(id, i, e))
  def completed[F[_], R <: Result.RetVal](id: Observation.Id, stepId: StepId, i: Int, r: Result.OK[R])
  : Event[F, Nothing, Nothing] = EventSystem[F](Completed(id, stepId, i, r))
  def stopCompleted[F[_], R <: Result.RetVal](id: Observation.Id, stepId: StepId, i: Int, r: Result.OKStopped[R])
  : Event[F, Nothing, Nothing] = EventSystem[F](StopCompleted(id, stepId, i, r))
  def aborted[F[_], R <: Result.RetVal](id: Observation.Id, stepId: StepId, i: Int, r: Result.OKAborted[R])
  : Event[F, Nothing, Nothing] = EventSystem[F](Aborted(id, stepId, i, r))
  def partial[F[_], R <: Result.PartialVal](id: Observation.Id, stepId: StepId, i: Int,
                                    r: Result.Partial[R]): Event[F, Nothing, Nothing] =
    EventSystem[F](PartialResult(id, stepId, i, r))
  def paused[F[_]](id: Observation.Id, i: Int, c: Result.Paused[F]): Event[F, Nothing, Nothing] = EventSystem[F](Paused[F](id, i, c))
  def breakpointReached[F[_]](id: Observation.Id): Event[F, Nothing, Nothing] = EventSystem[F](BreakpointReached(id))
  def busy[F[_]](id: Observation.Id, clientId: ClientId): Event[F, Nothing, Nothing] = EventSystem[F](Busy(id, clientId))
  def executed[F[_]](id: Observation.Id): Event[F, Nothing, Nothing] = EventSystem[F](Executed(id))
  def executing[F[_]](id: Observation.Id): Event[F, Nothing, Nothing] = EventSystem[F](Executing(id))
  def finished[F[_]](id: Observation.Id): Event[F, Nothing, Nothing] = EventSystem[F](Finished(id))
  def nullEvent[F[_]]: Event[F, Nothing, Nothing] = EventSystem[F](Null)
  def singleRunCompleted[F[_], R<:Result.RetVal](c: ActionCoords, r: Result.OK[R]): Event[F, Nothing, Nothing] =
    EventSystem[F](SingleRunCompleted(c, r))
  def singleRunFailed[F[_]](c: ActionCoords, e: Result.Error): Event[F, Nothing, Nothing] =
    EventSystem[F](SingleRunFailed(c, e))

}
