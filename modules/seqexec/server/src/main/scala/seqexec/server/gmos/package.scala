// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.engine.Result.PartialVal

package gmos {
  // N&S Partials. TODO add more details about the current step
  case object NSStart extends PartialVal
  case object NSStep extends PartialVal
  case object NSContinue extends PartialVal
  case object NSComplete extends PartialVal
}
