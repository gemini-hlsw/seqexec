// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import seqexec.engine.Result._
import seqexec.model.Model.ClientID

/**
  * Events generated internally by the Engine.
  */
sealed trait SystemEvent
final case class Completed[R<:RetVal](id: Sequence.Id, i: Int, r: OK[R]) extends SystemEvent
final case class PartialResult[R<:PartialVal](id: Sequence.Id, i: Int, r: Partial[R]) extends SystemEvent
final case class Paused[C <: PauseContext](id: Sequence.Id, i: Int, r: Result.Paused[C]) extends SystemEvent
final case class Failed(id: Sequence.Id, i: Int, e: Result.Error) extends SystemEvent
final case class Busy(id: Sequence.Id, clientId: ClientID) extends SystemEvent
final case class BreakpointReached(id: Sequence.Id) extends SystemEvent
final case class Executed(id: Sequence.Id) extends SystemEvent
final case class Executing(id: Sequence.Id) extends SystemEvent
final case class Finished(id: Sequence.Id) extends SystemEvent
object Null extends SystemEvent
