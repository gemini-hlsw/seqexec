// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.Config

trait System {
  val name: String

  /**
    * Called to configure a system, returns a Task[SeqexecFailure \/ ConfigResult]
    */
  def configure(config: Config): SeqAction[ConfigResult]
}

//Placeholder for config response
case class ConfigResult(sys: System)
