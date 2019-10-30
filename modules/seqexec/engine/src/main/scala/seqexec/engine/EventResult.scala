// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

sealed trait EventResult[U] extends Product with Serializable

object EventResult {
  sealed trait Outcome extends Product with Serializable

  object Outcome {
    case object Ok extends Outcome
    case object Failure extends Outcome
  }

  final case class UserCommandResponse[F[_], U](ue: UserEvent[F, _, U], outcome: Outcome, ud: Option[U]) extends EventResult[U]
  final case class SystemUpdate[F[_], U](se: SystemEvent[F], outcome: Outcome) extends EventResult[U]
}
