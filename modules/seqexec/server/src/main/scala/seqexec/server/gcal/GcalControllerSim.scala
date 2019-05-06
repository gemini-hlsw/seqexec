// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.effect.Sync
import cats.implicits._
import seqexec.server.gcal.GcalController.GcalConfig
import org.log4s.getLogger

object GcalControllerSim {
  def apply[F[_]: Sync]: GcalController[F] = new GcalController[F] {
    private val Log = getLogger

    override def getConfig: F[GcalConfig] =
      GcalController.GcalConfig.allOff.pure[F]

    override def applyConfig(config: GcalConfig): F[Unit] =
      Sync[F].delay(Log.debug("Simulating GCAL configuration")).void
  }
}
