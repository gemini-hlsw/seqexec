// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import lucuma.core.util.Enumerated

// UI model
sealed trait UserPromptResult extends Product with Serializable

object UserPromptResult {
  case object Cancel extends UserPromptResult
  case object Ok     extends UserPromptResult

  /** @group Typeclass Instances */
  implicit val SectionVisibilityStateEnumerated: Enumerated[UserPromptResult] =
    Enumerated.of(Cancel, Ok)

}
