package edu.gemini.seqexec.server
import edu.gemini.seqexec.server.GcalController.GcalConfig

/**
  * Created by jluhrs on 3/15/17.
  */
object GcalControllerSim extends GcalController {
  override def getConfig: SeqAction[GcalConfig] = SeqAction(GcalController.GcalConfig.allOff)

  override def applyConfig(config: GcalConfig): SeqAction[Unit] = SeqAction(())
}
