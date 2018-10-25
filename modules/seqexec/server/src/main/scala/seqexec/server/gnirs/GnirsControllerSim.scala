// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.effect.IO
import seqexec.model.dhs.ImageFileId
import seqexec.server.gnirs.GnirsController.GnirsConfig
import seqexec.server.{InstrumentControllerSim, ObserveCommand, Progress, SeqAction}
import squants.Time

object GnirsControllerSim extends GnirsController {

  private val sim: InstrumentControllerSim = InstrumentControllerSim(s"GNIRS")

  override def observe(fileId: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result] =
    sim.observe(fileId, expTime)

  override def applyConfig(config: GnirsConfig): SeqAction[Unit] = sim.applyConfig(config)

  override def stopObserve: SeqAction[Unit] = sim.stopObserve

  override def abortObserve: SeqAction[Unit] = sim.abortObserve

  override def endObserve: SeqAction[Unit] = sim.endObserve

  override def observeProgress(total: Time): fs2.Stream[IO, Progress] = sim.observeCountdown(total)
}
