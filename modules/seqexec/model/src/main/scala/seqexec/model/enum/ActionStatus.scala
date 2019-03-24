// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.Eq

sealed trait ActionStatus extends Product with Serializable

object ActionStatus {

  /** Action is not yet run. */
  case object Pending extends ActionStatus

  /** Action run and completed. */
  case object Completed extends ActionStatus

  /** Action currently running. */
  case object Running extends ActionStatus

  /** Action run but paused. */
  case object Paused extends ActionStatus

  /** Action run but failed to complete. */
  case object Failed extends ActionStatus

  implicit val equal: Eq[ActionStatus] =
    Eq.fromUniversalEquals

}
