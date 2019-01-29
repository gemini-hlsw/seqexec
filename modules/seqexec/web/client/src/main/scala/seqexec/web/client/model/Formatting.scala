// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import seqexec.model.enum.Instrument

object Formatting {

  def formatExposureTime(i: Instrument)(e: Double): String = i match {
    case Instrument.GmosN | Instrument.GmosS => f"$e%.0f"
    case _                                   => f"$e%.2f"
  }

  def formatExposure(i: Instrument)(v: Double): String =
    formatExposureTime(i)(v)

}
