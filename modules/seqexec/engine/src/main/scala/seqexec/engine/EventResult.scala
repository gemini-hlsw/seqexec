// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

sealed trait EventResult[D<:Engine.Types]

object EventResult {
  sealed trait Outcome
  case object Ok extends Outcome
  case object Failure extends Outcome
}

final case class UserCommandResponse[D<:Engine.Types](ue: UserEvent[D], outcome: EventResult.Outcome, ud: Option[D#EventData]) extends EventResult[D]
final case class SystemUpdate[D<:Engine.Types](se: SystemEvent, outcome: EventResult.Outcome) extends EventResult[D]

