// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.Sync
import cats.effect.Timer
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.SeqexecFailure.Execution
import seqexec.server.flamingos2.Flamingos2Controller.Flamingos2Config
import seqexec.server.InstrumentControllerSim
import seqexec.server.Progress
import org.log4s.getLogger
import squants.Time
import squants.time.TimeConversions._
import cats.implicits._
import fs2.Stream
import seqexec.server.InstrumentSystem.ElapsedTime

final case class Flamingos2ControllerSim[F[_]: Sync: Timer]()
    extends Flamingos2Controller[F] {
  private val sim = InstrumentControllerSim[F]("FLAMINGOS-2")

  override def observe(fileId:  ImageFileId,
                       expTime: Time): F[ObserveCommandResult] =
    sim.observe(fileId, expTime)

  override def applyConfig(config: Flamingos2Config): F[Unit] =
    sim.applyConfig(config)

  override def endObserve: F[Unit] = sim.endObserve

  override def observeProgress(total: Time): Stream[F, Progress] =
    sim.observeCountdown(total, ElapsedTime(0.seconds))
}

/**
  * This controller will run correctly but fail at step `failAt`
  */
final case class Flamingos2ControllerSimBad[F[_]: Sync: Timer](failAt: Int)
    extends Flamingos2Controller[F] {
  private val Log = getLogger

  private val sim = InstrumentControllerSim("FLAMINGOS-2 (bad)")

  private val counter: AtomicInteger = new AtomicInteger(0)

  override def observe(fileId:  ImageFileId,
                       expTime: Time): F[ObserveCommandResult] =
    sim.observe(fileId, expTime)

  override def applyConfig(config: Flamingos2Config): F[Unit] =
    Sync[F].delay {
      Log.info(s"Applying Flamingos-2 configuration $config")
    } *> Sync[F]
      .delay { counter.addAndGet(1) === failAt }
      .ifM({
        counter.set(0)
        Log.error(s"Error applying Flamingos-2 configuration")
        Sync[F].raiseError(Execution("simulated error"))
      }, Sync[F].unit)

  override def endObserve: F[Unit] = sim.endObserve

  override def observeProgress(total: Time): Stream[F, Progress] =
    sim.observeCountdown(total, ElapsedTime(0.seconds))
}
