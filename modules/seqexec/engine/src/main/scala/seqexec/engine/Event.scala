// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.implicits._
import fs2.Stream
import gem.Observation
import seqexec.model.ClientId
import seqexec.model.UserDetails
import seqexec.model.StepId
import seqexec.engine.SystemEvent._
import seqexec.engine.UserEvent._

/**
  * Anything that can go through the Event Queue.
  */
sealed trait Event[+F[_], +D <: Engine.Types] extends Product with Serializable

object Event {
  final case class EventUser[F[_], D <: Engine.Types](ue: UserEvent[F, D]) extends Event[F, D]
  final case class EventSystem[F[_]](se: SystemEvent[F]) extends Event[F, Nothing]

  def start[F[_], D <: Engine.Types](id: Observation.Id, user: UserDetails, clientId: ClientId, userCheck: D#StateType => Boolean): Event[F, D] =
    EventUser[F, D](Start[D](id, user.some, clientId, userCheck))
  def pause[F[_], D <: Engine.Types](id: Observation.Id, user: UserDetails): Event[F, D] = EventUser[F, D](Pause(id, user.some))
  def cancelPause[F[_], D <: Engine.Types](id: Observation.Id, user: UserDetails): Event[F, D] = EventUser[F, D](CancelPause(id, user.some))
  def breakpoint[F[_], D <: Engine.Types](id: Observation.Id, user: UserDetails, step: StepId, v: Boolean): Event[F, D] = EventUser[F, D](Breakpoint(id, user.some, step, v))
  def skip[F[_], D <: Engine.Types](id: Observation.Id, user: UserDetails, step: StepId, v: Boolean): Event[F, D] = EventUser[F, D](SkipMark(id, user.some, step, v))
  def poll(clientId: ClientId): Event[Nothing, Nothing] = EventUser[Nothing, Nothing](Poll(clientId))
  def getState[F[_], D <: Engine.Types](f: D#StateType => Option[Stream[F, Event[F, D]]]): Event[F, D] = EventUser[F, D](GetState(f))
  def modifyState[F[_], D <: Engine.Types](f: Handle[F, D#StateType, Event[F, D], D#EventData]): Event[F, D] = EventUser[F, D](ModifyState(f))
  def actionStop[F[_], D <: Engine.Types](id: Observation.Id, f: D#StateType => Option[Stream[F, Event[F, D]]]): Event[F, D] = EventUser[F, D](ActionStop(id, f))
  def actionResume[F[_], D <: Engine.Types](id: Observation.Id, i: Int, c: Stream[F, Result[F]]): Event[F, D] =
    EventUser[F, D](ActionResume(id, i, c))
  def logDebugMsg[F[_], D <: Engine.Types](msg: String): Event[F, D] = EventUser[F, D](LogDebug(msg))
  def logInfoMsg[F[_], D <: Engine.Types](msg: String): Event[F, D] = EventUser[F, D](LogInfo(msg))
  def logWarningMsg[F[_], D <: Engine.Types](msg: String): Event[F, D] = EventUser[F, D](LogWarning(msg))
  def logErrorMsg[F[_], D <: Engine.Types](msg: String): Event[F, D] = EventUser[F, D](LogError(msg))

  def failed[F[_]](id: Observation.Id, i: Int, e: Result.Error): Event[F, Nothing] = EventSystem[F](Failed(id, i, e))
  def completed[F[_], R <: Result.RetVal](id: Observation.Id, stepId: StepId, i: Int, r: Result.OK[R])
  : Event[F, Nothing] = EventSystem[F](Completed(id, stepId, i, r))
  def stopCompleted[F[_], R <: Result.RetVal](id: Observation.Id, stepId: StepId, i: Int, r: Result.OKStopped[R])
  : Event[F, Nothing] = EventSystem[F](StopCompleted(id, stepId, i, r))
  def partial[F[_], R <: Result.PartialVal](id: Observation.Id, stepId: StepId, i: Int,
                                    r: Result.Partial[R]): Event[F, Nothing] =
    EventSystem[F](PartialResult(id, stepId, i, r))
  def paused[F[_]](id: Observation.Id, i: Int, c: Result.Paused[F]): Event[F, Nothing] = EventSystem[F](Paused[F](id, i, c))
  def breakpointReached[F[_]](id: Observation.Id): Event[F, Nothing] = EventSystem[F](BreakpointReached(id))
  def busy[F[_]](id: Observation.Id, clientId: ClientId): Event[F, Nothing] = EventSystem[F](Busy(id, clientId))
  def executed[F[_]](id: Observation.Id): Event[F, Nothing] = EventSystem[F](Executed(id))
  def executing[F[_]](id: Observation.Id): Event[F, Nothing] = EventSystem[F](Executing(id))
  def finished[F[_]](id: Observation.Id): Event[F, Nothing] = EventSystem[F](Finished(id))
  def nullEvent[F[_]]: Event[F, Nothing] = EventSystem[F](Null)
  def singleRunCompleted[F[_], R<:Result.RetVal](c: ActionCoords, r: Result.OK[R]): Event[F, Nothing] =
    EventSystem[F](SingleRunCompleted(c, r))
  def singleRunFailed[F[_]](c: ActionCoords, e: Result.Error): Event[F, Nothing] =
    EventSystem[F](SingleRunFailed(c, e))

}
