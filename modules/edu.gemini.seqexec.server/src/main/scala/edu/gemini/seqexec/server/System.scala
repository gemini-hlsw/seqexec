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
final class ConfigResult(val sys: System)
object ConfigResult {
  def apply(sys: System): ConfigResult = new ConfigResult(sys)
}
