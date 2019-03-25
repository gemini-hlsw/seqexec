// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import seqexec.server.gcal.GcalController.GcalConfig
import seqexec.server.{SeqAction, TrySeq}
import org.log4s.getLogger

object GcalControllerSim extends GcalController {
  private val Log = getLogger

  override def getConfig: SeqAction[GcalConfig] = SeqAction(GcalController.GcalConfig.allOff)

  override def applyConfig(config: GcalConfig): SeqAction[Unit] = SeqAction.either {
    Log.debug("Simulating GCAL configuration")
    TrySeq(())
  }
}
