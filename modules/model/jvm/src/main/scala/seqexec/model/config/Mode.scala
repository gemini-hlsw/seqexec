// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.config

import lucuma.core.util.Enumerated

/**
 * Operating mode of the seqexec, development or production
 */
sealed trait Mode extends Product with Serializable

object Mode {
  case object Production  extends Mode
  case object Development extends Mode

  implicit val ModeEnumerated: Enumerated[Mode] =
    Enumerated.of(Production, Development)

}
