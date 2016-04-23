package edu.gemini.seqexec.web.server.model

import edu.gemini.seqexec.web.common.{SequenceSteps, Step, StepConfig}
import edu.gemini.spModel.config2.ConfigSequence

/**
  * Conversion of the legacy spmodel to the shared web model. Note that we don't want this class on the shared
  * bundle, to avoid polluting the JS with old Java code
  */
object Conversions {
  implicit class Config2Steps(val config: ConfigSequence) extends AnyVal {
    def toSequenceSteps: SequenceSteps = {
      val u = for {
        i <- 0 until config.size()
      } yield {
        val s = config.getStep(i)
        Step(i, s.itemEntries().toList.map(e => StepConfig(e.getKey.getPath, e.getItemValue.toString)))
      }
      SequenceSteps(u.toList)
    }
  }

}