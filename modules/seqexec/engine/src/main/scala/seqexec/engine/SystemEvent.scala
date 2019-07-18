// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import seqexec.engine.Result._
import seqexec.model.{ClientId, StepId}
import gem.Observation

/**
  * Events generated internally by the Engine.
  */
sealed trait SystemEvent[+F[_]] extends Product with Serializable

object SystemEvent {
  final case class Completed[R<:RetVal](id: Observation.Id, stepId: StepId, i: Int, r: OK[R])
    extends SystemEvent[Nothing]
  final case class StopCompleted[R<:RetVal](id: Observation.Id, stepId: StepId, i: Int, r: OKStopped[R])
    extends SystemEvent[Nothing]
  final case class PartialResult[R<:PartialVal](sid: Observation.Id, stepId: StepId, i: Int,
                                                r: Partial[R]) extends SystemEvent[Nothing]
  final case class Paused[F[_]](id: Observation.Id, i: Int, r: Result.Paused[F])
    extends SystemEvent[F]
  final case class Failed(id: Observation.Id, i: Int, e: Result.Error) extends SystemEvent[Nothing]
  final case class Busy(id: Observation.Id, clientId: ClientId) extends SystemEvent[Nothing]
  final case class BreakpointReached(id: Observation.Id) extends SystemEvent[Nothing]
  final case class Executed(id: Observation.Id) extends SystemEvent[Nothing]
  final case class Executing(id: Observation.Id) extends SystemEvent[Nothing]
  final case class Finished(id: Observation.Id) extends SystemEvent[Nothing]
  case object Null extends SystemEvent[Nothing]

  // Single action commands
  final case class SingleRunCompleted[R<:RetVal](actionCoords: ActionCoords, r: OK[R])
    extends SystemEvent[Nothing]
  final case class SingleRunFailed(actionCoords: ActionCoords, e: Result.Error)
    extends SystemEvent[Nothing]
}
