package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.{Config, ConfigSequence, ItemKey}
import edu.gemini.spModel.obscomp.InstConstants.INSTRUMENT_NAME_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

/**
 * Created by jluhrs on 4/28/15.
 */
sealed class Executor(sequenceConfig: ConfigSequence) {

  def run = runSequence(sequenceConfig.getAllSteps.toList)

  private def runSequence(config: List[Config]): Future[Int] = {
    def chainStep(config: List[Config], prevFuture: Future[Int]): Future[Int] = config match {
      case List() => prevFuture
      case x :: ys => chainStep(ys, prevFuture flatMap (i => runStep(x, i)))
    }
    config match {
      case List() => Promise.failed(new Exception("Empty sequence configuration")).future
      case x :: ys => chainStep(ys, runStep(x, 0))
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
    }) getOrElse Promise.failed(new Exception("Unrecognized instrument")).future
  }

}

object Executor {
  def apply(sequenceConfig: ConfigSequence) = new Executor(sequenceConfig)
}