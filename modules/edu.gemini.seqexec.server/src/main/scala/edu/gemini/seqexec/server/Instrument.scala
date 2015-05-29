package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.Config

import scalaz.concurrent.Task

/**
 * Created by jluhrs on 4/22/15.
 */
trait Instrument extends System {
  def observe(config: Config): Task[ObserveResult]
}

//Placeholder for observe response
final class ObserveResult(dataId: String)
object ObserveResult {
  def apply(dataId: String): ObserveResult = new ObserveResult(dataId)
}




