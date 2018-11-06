// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.effect.IO
import cats.implicits._
import fs2.Stream
import gem.Observation
import seqexec.model.ClientId
import seqexec.model.UserDetails

/**
  * Anything that can go through the Event Queue.
  */
sealed abstract class Event[+D<:Engine.Types]
final case class EventUser[D<:Engine.Types](ue: UserEvent[D]) extends Event[D]
final case class EventSystem(se: SystemEvent) extends Event[Nothing]

object Event {

  def start[D<:Engine.Types](id: Observation.Id, user: UserDetails, clientId: ClientId, userCheck: D#StateType => Boolean): Event[D] = EventUser[D](Start[D](id, user.some, clientId, userCheck))
  def pause[D<:Engine.Types](id: Observation.Id, user: UserDetails): Event[D] = EventUser[D](Pause(id, user.some))
  def cancelPause[D<:Engine.Types](id: Observation.Id, user: UserDetails): Event[D] = EventUser[D](CancelPause(id, user.some))
  def breakpoint[D<:Engine.Types](id: Observation.Id, user: UserDetails, step: Step.Id, v: Boolean): Event[D] = EventUser[D](Breakpoint(id, user.some, step, v))
  def skip[D<:Engine.Types](id: Observation.Id, user: UserDetails, step: Step.Id, v: Boolean): Event[D] = EventUser[D](SkipMark(id, user.some, step, v))
  def poll(clientId: ClientId): Event[Nothing] = EventUser(Poll(clientId))
  def getState[D<:Engine.Types](f: D#StateType => Option[Stream[IO, Event[D]]]): Event[D] = EventUser[D](GetState[D](f))
  def modifyState[D<:Engine.Types](f: Handle[D#StateType, Event[D], D#EventData]): Event[D] = EventUser[D](ModifyState[D](f))
  def actionStop[D<:Engine.Types](id: Observation.Id, f: D#StateType => Option[Stream[IO, Event[D]]]): Event[D] = EventUser[D](ActionStop(id, f))
  def actionResume[D<:Engine.Types](id: Observation.Id, i: Int, c: Stream[IO, Result]): Event[D] =
    EventUser[D](ActionResume(id, i, c))
  def logDebugMsg[D<:Engine.Types](msg: String): Event[D] = EventUser[D](LogDebug(msg))
  def logInfoMsg[D<:Engine.Types](msg: String): Event[D] = EventUser[D](LogInfo(msg))
  def logWarningMsg[D<:Engine.Types](msg: String): Event[D] = EventUser[D](LogWarning(msg))
  def logErrorMsg[D<:Engine.Types](msg: String): Event[D] = EventUser[D](LogError(msg))

  def failed(id: Observation.Id, i: Int, e: Result.Error): Event[Nothing] = EventSystem(Failed(id, i, e))
  def completed[R<:Result.RetVal](id: Observation.Id, i: Int, r: Result.OK[R]): Event[Nothing] = EventSystem(Completed(id, i, r))
  def partial[R<:Result.PartialVal](id: Observation.Id, i: Int, r: Result.Partial[R]): Event[Nothing] = EventSystem(PartialResult(id, i, r))
  def paused[C <: Result.PauseContext](id: Observation.Id, i: Int, c: Result.Paused[C]): Event[Nothing] = EventSystem(Paused(id, i, c))
  def breakpointReached(id: Observation.Id): Event[Nothing] = EventSystem(BreakpointReached(id))
  def busy(id: Observation.Id, clientId: ClientId): Event[Nothing] = EventSystem(Busy(id, clientId))
  def executed(id: Observation.Id): Event[Nothing] = EventSystem(Executed(id))
  def executing(id: Observation.Id): Event[Nothing] = EventSystem(Executing(id))
  def finished(id: Observation.Id): Event[Nothing] = EventSystem(Finished(id))
  def nullEvent: Event[Nothing] = EventSystem(Null)

}
