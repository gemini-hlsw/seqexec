// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.engine

import scalaz.syntax.std.option._
import edu.gemini.seqexec.model.Model.{CloudCover, Conditions, ImageQuality, Observer, Operator, SkyBackground, WaterVapor}
import edu.gemini.seqexec.model.UserDetails

import scalaz.concurrent.Task
import scalaz.stream.Process

/**
  * Anything that can go through the Event Queue.
  */
sealed trait Event
final case class EventUser(ue: UserEvent) extends Event
final case class EventSystem(se: SystemEvent) extends Event

object Event {

  def start(id: Sequence.Id, user: UserDetails): Event = EventUser(Start(id, user.some))
  def pause(id: Sequence.Id, user: UserDetails): Event = EventUser(Pause(id, user.some))
  def cancelPause(id: Sequence.Id, user: UserDetails): Event = EventUser(CancelPause(id, user.some))
  def load(id: Sequence.Id, sequence: Sequence): Event = EventUser(Load(id, sequence))
  def unload(id: Sequence.Id): Event = EventUser(Unload(id))
  def breakpoint(id: Sequence.Id, user: UserDetails, step: Step.Id, v: Boolean): Event = EventUser(Breakpoint(id, user.some, step, v))
  def skip(id: Sequence.Id, user: UserDetails, step: Step.Id, v: Boolean): Event = EventUser(SkipMark(id, user.some, step, v))
  def setOperator(name: Operator, user: UserDetails): Event = EventUser(SetOperator(name, user.some))
  def setObserver(id: Sequence.Id, user: UserDetails, name: Observer): Event = EventUser(SetObserver(id, user.some, name))
  def setConditions(conditions: Conditions, user: UserDetails): Event = EventUser(SetConditions(conditions, user.some))
  def setImageQuality(iq: ImageQuality, user: UserDetails): Event = EventUser(SetImageQuality(iq, user.some))
  def setWaterVapor(wv: WaterVapor, user: UserDetails): Event = EventUser(SetWaterVapor(wv, user.some))
  def setSkyBackground(sb: SkyBackground, user: UserDetails): Event = EventUser(SetSkyBackground(sb, user.some))
  def setCloudCover(cc: CloudCover, user: UserDetails): Event = EventUser(SetCloudCover(cc, user.some))
  val poll: Event = EventUser(Poll)
  def getState(f: (Engine.State) => Task[Option[Process[Task, Event]]]): Event = EventUser(GetState(f))
  def getSeqState(id: Sequence.Id, f: (Sequence.State) => Option[Process[Task, Event]]): Event = EventUser(GetSeqState(id, f))
  def actionStop(id: Sequence.Id, f: (Sequence.State) => Option[Process[Task, Event]]): Event = EventUser(ActionStop(id, f))
  def actionResume(id: Sequence.Id, i: Int, c: Task[Result]): Event = EventUser(ActionResume(id, i, c))
  def logDebugMsg(msg: String): Event = EventUser(LogDebug(msg))
  def logInfoMsg(msg: String): Event = EventUser(LogInfo(msg))
  def logWarningMsg(msg: String): Event = EventUser(LogWarning(msg))
  def logErrorMsg(msg: String): Event = EventUser(LogError(msg))

  def failed(id: Sequence.Id, i: Int, e: Result.Error): Event = EventSystem(Failed(id, i, e))
  def completed[R<:Result.RetVal](id: Sequence.Id, i: Int, r: Result.OK[R]): Event = EventSystem(Completed(id, i, r))
  def partial[R<:Result.PartialVal](id: Sequence.Id, i: Int, r: Result.Partial[R]): Event = EventSystem(PartialResult(id, i, r))
  def paused[C <: Result.PauseContext](id: Sequence.Id, i: Int, c: Result.Paused[C]): Event = EventSystem(Paused(id, i, c))
  def breakpointReached(id: Sequence.Id): Event = EventSystem(BreakpointReached(id))
  def busy(id: Sequence.Id): Event = EventSystem(Busy(id))
  def executed(id: Sequence.Id): Event = EventSystem(Executed(id))
  def executing(id: Sequence.Id): Event = EventSystem(Executing(id))
  def finished(id: Sequence.Id): Event = EventSystem(Finished(id))
  def nullEvent: Event = EventSystem(Null)

}
