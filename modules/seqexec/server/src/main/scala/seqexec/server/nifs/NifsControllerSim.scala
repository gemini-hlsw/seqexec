// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.effect.IO
import seqexec.model.dhs.ImageFileId
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.nifs.NifsController.DCConfig
import seqexec.server.nifs.NifsController.NifsConfig
import seqexec.server.InstrumentControllerSim
import seqexec.server.ObserveCommand
import seqexec.server.Progress
import squants.Time
import squants.time.TimeConversions._

object NifsControllerSim extends NifsController[IO] {

  private val sim: InstrumentControllerSim = InstrumentControllerSim(s"NIRI")

  override def observe(fileId: ImageFileId,
                       cfg:    DCConfig): IO[ObserveCommand.Result] =
    calcTotalExposureTime(cfg).flatMap {ot =>
      sim
        .observe(fileId, ot)
        .getOrElse(ObserveCommand.Aborted)
    }

  override def applyConfig(config: NifsConfig): IO[Unit] =
    sim.applyConfig(config).liftF

  override def stopObserve: IO[Unit] = sim.stopObserve.liftF

  override def abortObserve: IO[Unit] = sim.abortObserve.liftF

  override def endObserve: IO[Unit] = sim.endObserve.liftF

  override def observeProgress(total: Time): fs2.Stream[IO, Progress] =
    sim.observeCountdown(total, ElapsedTime(0.seconds)).streamLiftIO[IO]

}
