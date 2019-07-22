// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.effect.IO
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
  final case class EventUser[D <: Engine.Types](ue: UserEvent[D]) extends Event[Nothing, D]
  final case class EventSystem[F[_]](se: SystemEvent[F]) extends Event[F, Nothing]

  def start[D <: Engine.Types](id: Observation.Id, user: UserDetails, clientId: ClientId, userCheck: D#StateType => Boolean): Event[IO, D] =
    EventUser[D](Start[D](id, user.some, clientId, userCheck))
  def pause[D <: Engine.Types](id: Observation.Id, user: UserDetails): Event[IO, D] = EventUser[D](Pause(id, user.some))
  def cancelPause[D <: Engine.Types](id: Observation.Id, user: UserDetails): Event[IO, D] = EventUser[D](CancelPause(id, user.some))
  def breakpoint[D <: Engine.Types](id: Observation.Id, user: UserDetails, step: StepId, v: Boolean): Event[IO, D] = EventUser[D](Breakpoint(id, user.some, step, v))
  def skip[D <: Engine.Types](id: Observation.Id, user: UserDetails, step: StepId, v: Boolean): Event[IO, D] = EventUser[D](SkipMark(id, user.some, step, v))
  def poll(clientId: ClientId): Event[Nothing, Nothing] = EventUser(Poll(clientId))
  def getState[D <: Engine.Types](f: D#StateType => Option[Stream[IO, Event[IO, D]]]): Event[IO, D] = EventUser[D](GetState[D](f))
  def modifyState[D <: Engine.Types](f: Handle[D#StateType, Event[IO, D], D#EventData]): Event[IO, D] = EventUser[D](ModifyState[D](f))
  def actionStop[D <: Engine.Types](id: Observation.Id, f: D#StateType => Option[Stream[IO, Event[IO, D]]]): Event[IO, D] = EventUser[D](ActionStop(id, f))
  def actionResume[D <: Engine.Types](id: Observation.Id, i: Int, c: Stream[IO, Result[IO]]): Event[IO, D] =
    EventUser[D](ActionResume(id, i, c))
  def logDebugMsg[D <: Engine.Types](msg: String): Event[IO, D] = EventUser[D](LogDebug(msg))
  def logInfoMsg[D <: Engine.Types](msg: String): Event[IO, D] = EventUser[D](LogInfo(msg))
  def logWarningMsg[D <: Engine.Types](msg: String): Event[IO, D] = EventUser[D](LogWarning(msg))
  def logErrorMsg[D <: Engine.Types](msg: String): Event[IO, D] = EventUser[D](LogError(msg))

  def failed(id: Observation.Id, i: Int, e: Result.Error): Event[IO, Nothing] = EventSystem[IO](Failed(id, i, e))
  def completed[R<:Result.RetVal](id: Observation.Id, stepId: StepId, i: Int, r: Result.OK[R])
  : Event[IO, Nothing] = EventSystem[IO](Completed(id, stepId, i, r))
  def stopCompleted[R<:Result.RetVal](id: Observation.Id, stepId: StepId, i: Int, r: Result.OKStopped[R])
  : Event[IO, Nothing] = EventSystem[IO](StopCompleted(id, stepId, i, r))
  def partial[R<:Result.PartialVal](id: Observation.Id, stepId: StepId, i: Int,
                                    r: Result.Partial[R]): Event[IO, Nothing] =
    EventSystem[IO](PartialResult(id, stepId, i, r))
  def paused[F[_]](id: Observation.Id, i: Int, c: Result.Paused[F]): Event[F, Nothing] = EventSystem[F](Paused[F](id, i, c))
  def breakpointReached(id: Observation.Id): Event[IO, Nothing] = EventSystem[IO](BreakpointReached(id))
  def busy(id: Observation.Id, clientId: ClientId): Event[IO, Nothing] = EventSystem[IO](Busy(id, clientId))
  def executed(id: Observation.Id): Event[IO, Nothing] = EventSystem[IO](Executed(id))
  def executing(id: Observation.Id): Event[IO, Nothing] = EventSystem[IO](Executing(id))
  def finished(id: Observation.Id): Event[IO, Nothing] = EventSystem[IO](Finished(id))
  def nullEvent: Event[IO, Nothing] = EventSystem[IO](Null)
  def singleRunCompleted[R<:Result.RetVal](c: ActionCoords, r: Result.OK[R]): Event[IO, Nothing] =
    EventSystem[IO](SingleRunCompleted(c, r))
  def singleRunFailed(c:ActionCoords, e: Result.Error): Event[IO, Nothing] =
    EventSystem[IO](SingleRunFailed(c, e))

}
