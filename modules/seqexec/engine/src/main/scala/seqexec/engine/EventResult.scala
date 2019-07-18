// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

sealed trait EventResult[D<:Engine.Types] extends Product with Serializable

object EventResult {
  sealed trait Outcome extends Product with Serializable

  object Outcome {
    case object Ok extends Outcome
    case object Failure extends Outcome
  }

  final case class UserCommandResponse[D<:Engine.Types](ue: UserEvent[D], outcome: Outcome, ud: Option[D#EventData]) extends EventResult[D]
  final case class SystemUpdate[F[_], D<:Engine.Types](se: SystemEvent[F], outcome: Outcome) extends EventResult[D]
}
