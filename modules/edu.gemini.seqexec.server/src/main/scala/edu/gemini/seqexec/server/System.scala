package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.Config

import scala.concurrent.Future

/**
 * Created by jluhrs on 4/22/15.
 */
trait System {
  val name: String

  def configure(config: Config): Future[Unit]
}
