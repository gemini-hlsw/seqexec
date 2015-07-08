package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.Config

import scalaz.concurrent.Task

/**
 * Created by jluhrs on 4/22/15.
 */
trait System {
  val name: String

  def configure(config: Config): SeqAction[ConfigResult]
}

//Placeholder for config response
case class ConfigResult(sys: System)
