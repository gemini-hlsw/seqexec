// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import gem.util.Enumerated

// UI model
sealed trait SectionVisibilityState extends Product with Serializable

object SectionVisibilityState {
  case object SectionOpen extends SectionVisibilityState
  case object SectionClosed extends SectionVisibilityState

  /** @group Typeclass Instances */
  implicit val SectionVisibilityStateEnumerated: Enumerated[SectionVisibilityState] =
    Enumerated.of(SectionOpen, SectionClosed)

}
