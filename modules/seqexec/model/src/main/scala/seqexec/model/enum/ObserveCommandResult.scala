// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import gem.util.Enumerated

sealed trait ObserveCommandResult extends Product with Serializable

object ObserveCommandResult {
  case object Success extends ObserveCommandResult
  case object Paused  extends ObserveCommandResult
  case object Stopped extends ObserveCommandResult
  case object Aborted extends ObserveCommandResult

  /** @group Typeclass Instances */
  implicit val ObserveCommandResultEnumerated: Enumerated[ObserveCommandResult] =
    Enumerated.of(Success, Paused, Stopped, Aborted)
}
