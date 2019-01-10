// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import cats.effect.IO
import seqexec.model.dhs.ImageFileId
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.niri.NiriController.{DCConfig, NiriConfig}
import seqexec.server.{InstrumentControllerSim, ObserveCommand, Progress, SeqAction}
import squants.Time
import squants.time.TimeConversions._

object NiriControllerSim extends NiriController {

  private val sim: InstrumentControllerSim = InstrumentControllerSim(s"NIRI")

  override def observe(fileId: ImageFileId, cfg: DCConfig): SeqAction[ObserveCommand.Result] =
    sim.observe(fileId, calcTotalExposureTime(cfg))

  override def applyConfig(config: NiriConfig): SeqAction[Unit] = sim.applyConfig(config)

  override def stopObserve: SeqAction[Unit] = sim.stopObserve

  override def abortObserve: SeqAction[Unit] = sim.abortObserve

  override def endObserve: SeqAction[Unit] = sim.endObserve

  override def observeProgress(total: Time): fs2.Stream[IO, Progress] =
    sim.observeCountdown(total, ElapsedTime(0.seconds))

  override def calcTotalExposureTime(cfg: DCConfig): Time = {
    val MinIntTime = 0.5.seconds

    (cfg.exposureTime + MinIntTime) * cfg.coadds.toDouble
  }
}
