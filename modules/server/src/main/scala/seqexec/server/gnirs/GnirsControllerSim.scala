// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.effect.Async
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.InstrumentControllerSim
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.Progress
import seqexec.server.gnirs.GnirsController.DCConfig
import seqexec.server.gnirs.GnirsController.GnirsConfig
import seqexec.server.keywords.GdsClient
import squants.Time
import squants.time.TimeConversions._

object GnirsControllerSim {
  private val name = "GNIRS"

  def apply[F[_]: Logger: Async](client: GdsClient[F]): F[GnirsController[F]] =
    InstrumentControllerSim[F](name).map { sim =>
      new GnirsController[F] {

        override def gdsClient: GdsClient[F] = client

        override def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult] =
          sim.observe(fileId, expTime)

        override def applyConfig(config: GnirsConfig): F[Unit] = sim.applyConfig(config)

        override def stopObserve: F[Unit] = sim.stopObserve

        override def abortObserve: F[Unit] = sim.abortObserve

        override def endObserve: F[Unit] = sim.endObserve

        override def observeProgress(total: Time): fs2.Stream[F, Progress] =
          sim.observeCountdown(total, ElapsedTime(0.seconds))

        override def calcTotalExposureTime(cfg: DCConfig): F[Time] =
          GnirsController.calcTotalExposureTime[F](cfg)
      }
    }
}
