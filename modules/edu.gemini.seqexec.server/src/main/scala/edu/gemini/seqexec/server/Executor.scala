package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.{Config, ConfigSequence, ItemKey}
import edu.gemini.spModel.obscomp.InstConstants.INSTRUMENT_NAME_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by jluhrs on 4/28/15.
 */
final class Executor(sequenceConfig: ConfigSequence) {

  def run: Future[Int] = runSequence(sequenceConfig.getAllSteps.toList)

  private def runSequence(config: List[Config]): Future[Int] = {
    config match {
      case Nil => Future.failed(new Exception("Empty sequence configuration"))
      case _ => config.foldLeft(Future(0))((f, s) => f.flatMap(runStep(s, _)))
    }
  }

  private def runStep(stepConfig: Config, stepIdx: Int): Future[Int] = {
    val instrument = stepConfig.getItemValue(new ItemKey(INSTRUMENT_KEY, INSTRUMENT_NAME_PROP)) match {
      case GMOS_S.name => Some(GMOS_S)
      case _ => None
    }

    instrument map (a => {
      val systems: List[System] = List(TCS, a)

      Future.sequence(systems map (x => x.configure(stepConfig))) flatMap
        (x => a.observe(stepConfig)) map (x => stepIdx + 1)
    }) getOrElse Future.failed(new Exception("Unrecognized instrument"))
  }

}

object Executor {
  def apply(sequenceConfig: ConfigSequence): Executor = new Executor(sequenceConfig)
}
