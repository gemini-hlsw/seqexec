package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.Config

import scala.concurrent.Future

/**
 * Created by jluhrs on 4/22/15.
 */
trait Instrument extends System {
  def observe(config: Config): Future[Unit]
}

