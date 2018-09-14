// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._

// UI model
sealed trait SectionVisibilityState extends Product with Serializable
case object SectionOpen extends SectionVisibilityState
case object SectionClosed extends SectionVisibilityState

object SectionVisibilityState {
  implicit val eq: Eq[SectionVisibilityState] = Eq.fromUniversalEquals
}
