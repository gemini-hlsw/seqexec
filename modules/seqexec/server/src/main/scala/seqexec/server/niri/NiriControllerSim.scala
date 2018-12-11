package seqexec.server.niri

import cats.effect.IO
import seqexec.model.dhs.ImageFileId
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.niri.NiriController.NiriConfig
import seqexec.server.{InstrumentControllerSim, ObserveCommand, Progress, SeqAction}
import squants.Time
import squants.time.TimeConversions._

class NiriControllerSim extends NiriController {

  private val sim: InstrumentControllerSim = InstrumentControllerSim(s"NIRI")

  override def observe(fileId: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result] =
    sim.observe(fileId, expTime)

  override def applyConfig(config: NiriConfig): SeqAction[Unit] = sim.applyConfig(config)

  override def stopObserve: SeqAction[Unit] = sim.stopObserve

  override def abortObserve: SeqAction[Unit] = sim.abortObserve

  override def endObserve: SeqAction[Unit] = sim.endObserve

  override def observeProgress(total: Time): fs2.Stream[IO, Progress] =
    sim.observeCountdown(total, ElapsedTime(0.seconds))

}
