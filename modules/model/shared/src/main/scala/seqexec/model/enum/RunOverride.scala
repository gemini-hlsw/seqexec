// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import lucuma.core.util.Enumerated

sealed trait RunOverride extends Product with Serializable

object RunOverride {

  /** Default case, do regular checks */
  case object Default extends RunOverride

  /** Override the checks and try to run anyway */
  case object Override extends RunOverride

  /** @group Typeclass Instances */
  implicit val RunOverrideEnumerated: Enumerated[RunOverride] =
    Enumerated.of(Default, Override)
}
