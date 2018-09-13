// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.Eq

sealed trait BatchCommandState extends Product with Serializable

object BatchCommandState {
  case object Idle extends BatchCommandState
  case object Run extends BatchCommandState
  case object Stop extends BatchCommandState

  implicit val equal: Eq[BatchCommandState] = Eq.fromUniversalEquals
}
