// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.engine

import edu.gemini.seqexec.engine.Result.{OK, Partial, PartialVal, RetVal}

/**
  * Created by jluhrs on 9/25/17.
  */
/**
  * Events generated internally by the Engine.
  */
sealed trait SystemEvent
final case class Completed[R<:RetVal](id: Sequence.Id, i: Int, r: OK[R]) extends SystemEvent
final case class PartialResult[R<:PartialVal](id: Sequence.Id, i: Int, r: Partial[R]) extends SystemEvent
final case class Paused(id: Sequence.Id, i: Int) extends SystemEvent
final case class Failed(id: Sequence.Id, i: Int, e: Result.Error) extends SystemEvent
final case class Busy(id: Sequence.Id) extends SystemEvent
final case class Executed(id: Sequence.Id) extends SystemEvent
final case class Executing(id: Sequence.Id) extends SystemEvent
final case class Finished(id: Sequence.Id) extends SystemEvent
final object Null extends SystemEvent

