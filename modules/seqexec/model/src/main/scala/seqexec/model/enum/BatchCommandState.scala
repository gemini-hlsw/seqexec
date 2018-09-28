// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.Eq
import seqexec.model.ClientID

sealed trait BatchCommandState extends Product with Serializable

object BatchCommandState {
  case object Idle extends BatchCommandState
  final case class Run(clientId: ClientID) extends BatchCommandState
  case object Stop extends BatchCommandState

  implicit val equal: Eq[BatchCommandState] = Eq.fromUniversalEquals
}
