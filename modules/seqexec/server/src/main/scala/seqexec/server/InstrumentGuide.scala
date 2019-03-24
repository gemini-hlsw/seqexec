// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.model.enum.Instrument
import squants.space.Length

trait InstrumentGuide {
  def instrument: Instrument
  def oiOffsetGuideThreshold: Option[Length]
}
