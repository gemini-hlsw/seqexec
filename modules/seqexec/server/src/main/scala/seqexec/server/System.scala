// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.model.Model.Resource
import edu.gemini.spModel.config2.Config

trait System[F[_]] {
  val resource: Resource

  /**
    * Called to configure a system
    */
  def configure(config: Config): SeqActionF[F, ConfigResult[F]]

  def notifyObserveStart: SeqActionF[F, Unit]

  def notifyObserveEnd: SeqActionF[F, Unit]
}

//Placeholder for config response
final case class ConfigResult[F[_]](sys: System[F])
