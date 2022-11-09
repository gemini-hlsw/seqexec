// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import cats.effect.Async
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.InstrumentControllerSim
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.Progress
import seqexec.server.niri.NiriController.DCConfig
import seqexec.server.niri.NiriController.NiriConfig
import squants.Time
import squants.time.TimeConversions._

object NiriControllerSim {
  def apply[F[_]: Async: Logger]: F[NiriController[F]] =
    InstrumentControllerSim[F](s"NIRI").map { sim =>
      new NiriController[F] {
        override def observe(fileId: ImageFileId, cfg: DCConfig): F[ObserveCommandResult] =
          calcTotalExposureTime(cfg).flatMap(sim.observe(fileId, _))

        override def applyConfig(config: NiriConfig): F[Unit] = sim.applyConfig(config)

        override def stopObserve: F[Unit] = sim.stopObserve

        override def abortObserve: F[Unit] = sim.abortObserve

        override def endObserve: F[Unit] = sim.endObserve

        override def observeProgress(total: Time): fs2.Stream[F, Progress] =
          sim.observeCountdown(total, ElapsedTime(0.seconds))

        override def calcTotalExposureTime(cfg: DCConfig): F[Time] = {
          val MinIntTime = 0.5.seconds

          (cfg.exposureTime + MinIntTime) * cfg.coadds.toDouble
        }.pure[F]
      }
    }

}
