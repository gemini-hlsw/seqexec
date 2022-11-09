// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.effect.Async
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.InstrumentControllerSim
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.Progress
import seqexec.server.nifs.NifsController.DCConfig
import seqexec.server.nifs.NifsController.NifsConfig
import squants.Time
import squants.time.TimeConversions._

object NifsControllerSim {
  def apply[F[_]: Async: Logger]: F[NifsController[F]] =
    InstrumentControllerSim[F](s"NIFS").map { sim =>
      new NifsController[F] {

        override def observe(fileId: ImageFileId, cfg: DCConfig): F[ObserveCommandResult] =
          sim.observe(fileId, calcTotalExposureTime(cfg))

        override def applyConfig(config: NifsConfig): F[Unit] =
          sim.applyConfig(config)

        override def stopObserve: F[Unit] = sim.stopObserve

        override def abortObserve: F[Unit] = sim.abortObserve

        override def endObserve: F[Unit] = sim.endObserve

        override def observeProgress(total: Time): fs2.Stream[F, Progress] =
          sim.observeCountdown(total, ElapsedTime(0.seconds))

        override def calcTotalExposureTime(cfg: DCConfig): Time =
          NifsController.calcTotalExposureTime[F](cfg)

      }
    }
}
