// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import org.log4s.getLogger

import edu.gemini.seqexec.server.GcalController.GcalConfig

/**
  * Created by jluhrs on 3/15/17.
  */
object GcalControllerSim extends GcalController {
  private val Log = getLogger

  override def getConfig: SeqAction[GcalConfig] = SeqAction(GcalController.GcalConfig.allOff)

  def printConfig(config: GcalConfig): List[String] = List(
    "lampAr = " + config.lampAr,
    "lampCuar = " + config.lampCuAr,
    "lampQH = " + config.lampQh,
    "lampThAr = " + config.lampThAr,
    "lampXe = " + config.lampXe,
    "lampIr = " + config.lampIr,
    "shutter = " + config.shutter,
    "filter = " + config.filter,
    "diffuser = " + config.diffuser
    )

  override def applyConfig(config: GcalConfig): SeqAction[Unit] = SeqAction.either {
    Log.info("applyConfig: config is\n" + printConfig(config).mkString("\n"))
    TrySeq(())
  }
}
