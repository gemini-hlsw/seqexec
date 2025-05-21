// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import lucuma.core.util.Enumerated

// UI model
sealed trait SectionVisibilityState extends Product with Serializable {
  def flip: SectionVisibilityState = this match {
    case SectionVisibilityState.SectionOpen   => SectionVisibilityState.SectionClosed
    case SectionVisibilityState.SectionClosed => SectionVisibilityState.SectionOpen
  }
}

object SectionVisibilityState {
  case object SectionOpen   extends SectionVisibilityState
  case object SectionClosed extends SectionVisibilityState

  /** @group Typeclass Instances */
  implicit val SectionVisibilityStateEnumerated: Enumerated[SectionVisibilityState] =
    Enumerated.of(SectionOpen, SectionClosed)

}
