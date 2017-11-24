// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.seqexec.model.Model.Resource
import edu.gemini.spModel.config2.Config

trait System {
  val resource: Resource

  /**
    * Called to configure a system, returns a Task[SeqexecFailure \/ ConfigResult]
    */
  def configure(config: Config): SeqAction[ConfigResult]

  def notifyObserveStart: SeqAction[Unit]

  def notifyObserveEnd: SeqAction[Unit]
}

//Placeholder for config response
final case class ConfigResult(sys: System)
