// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.model.enum.Resource
import edu.gemini.spModel.config2.Config

trait System[F[_]] {
  val resource: Resource

  /**
    * Called to configure a system
    */
  def configure(config: Config): F[ConfigResult[F]]

  def notifyObserveStart: F[Unit]

  def notifyObserveEnd: F[Unit]
}

//Placeholder for config response
final case class ConfigResult[F[_]](sys: System[F])
