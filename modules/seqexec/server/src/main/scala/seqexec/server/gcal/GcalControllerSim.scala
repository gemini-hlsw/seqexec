// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import io.chrisdavenport.log4cats.Logger
import seqexec.server.gcal.GcalController.GcalConfig

object GcalControllerSim {
  def apply[F[_]: Logger]: GcalController[F] = new GcalController[F] {
    override def applyConfig(config: GcalConfig): F[Unit] =
      Logger[F].debug("Simulating GCAL configuration")
  }
}
