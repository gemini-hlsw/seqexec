package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.Config

import scalaz.concurrent.Task

/**
 * Created by jluhrs on 4/22/15.
 */
trait Instrument extends System {
  val sfName: String
  def observe(config: Config): SeqAction[ObserveResult]
}

//Placeholder for observe response
case class ObserveResult(dataId: String)

