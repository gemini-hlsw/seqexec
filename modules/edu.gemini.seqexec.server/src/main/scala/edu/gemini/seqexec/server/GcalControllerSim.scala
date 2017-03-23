package edu.gemini.seqexec.server
import java.util.logging.Logger

import edu.gemini.seqexec.server.GcalController.{ArLampState, GcalConfig, LampState}

/**
  * Created by jluhrs on 3/15/17.
  */
object GcalControllerSim extends GcalController {
  private val Log = Logger.getLogger(getClass.getName)

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

  override def applyConfig(config: GcalConfig): SeqAction[Unit] = SeqAction.either{
    Log.info("applyConfig: config is\n" + printConfig(config).mkString("\n"))
    TrySeq(())
  }
}
