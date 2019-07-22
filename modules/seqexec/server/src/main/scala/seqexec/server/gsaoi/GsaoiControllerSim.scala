// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import cats.effect.Sync
import cats.effect.Timer
import cats.implicits._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.gsaoi.GsaoiController.DCConfig
import seqexec.server.gsaoi.GsaoiController.GsaoiConfig
import seqexec.server.InstrumentControllerSim
import seqexec.server.Progress
import squants.Time
import squants.time.TimeConversions._

object GsaoiControllerSim {
  def apply[F[_]: Sync: Timer]: GsaoiController[F] =
    new GsaoiController[F] {

      private val sim: InstrumentControllerSim[F] = InstrumentControllerSim[F](s"GSAOI")

      override def observe(fileId: ImageFileId,
                           cfg:    DCConfig): F[ObserveCommandResult] =
        calcTotalExposureTime(cfg).flatMap {
          sim.observe(fileId, _)
        }

      override def applyConfig(config: GsaoiConfig): F[Unit] =
        sim.applyConfig(config)

      override def stopObserve: F[Unit] = sim.stopObserve

      override def abortObserve: F[Unit] = sim.abortObserve

      override def endObserve: F[Unit] = sim.endObserve

      override def observeProgress(total: Time): fs2.Stream[F, Progress] =
        sim.observeCountdown(total, ElapsedTime(0.seconds))

    }

}
