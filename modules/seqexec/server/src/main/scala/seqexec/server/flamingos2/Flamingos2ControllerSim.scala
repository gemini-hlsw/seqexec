// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import cats.MonadError
import cats.effect.Sync
import cats.effect.Timer
import cats.effect.concurrent.Ref
import cats.implicits._
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.SeqexecFailure.Execution
import seqexec.server.flamingos2.Flamingos2Controller.Flamingos2Config
import seqexec.server.InstrumentControllerSim
import seqexec.server.Progress
import squants.Time
import squants.time.TimeConversions._

final case class Flamingos2ControllerSim[F[_]] private (sim: InstrumentControllerSim[F])
    extends Flamingos2Controller[F] {

  override def observe(fileId:  ImageFileId,
                       expTime: Time): F[ObserveCommandResult] =
    sim.observe(fileId, expTime)

  override def applyConfig(config: Flamingos2Config): F[Unit] =
    sim.applyConfig(config)

  override def endObserve: F[Unit] = sim.endObserve

  override def observeProgress(total: Time): Stream[F, Progress] =
    sim.observeCountdown(total, ElapsedTime(0.seconds))
}

object Flamingos2ControllerSim {
  def apply[F[_]: Sync: Logger: Timer]: F[Flamingos2Controller[F]] =
    InstrumentControllerSim("FLAMINGOS-2").map(Flamingos2ControllerSim(_))

}

/**
  * This controller will run correctly but fail at step `failAt`
  */
final case class Flamingos2ControllerSimBad[F[_]: MonadError[?[_], Throwable]: Logger] private (failAt: Int, sim: InstrumentControllerSim[F], counter: Ref[F, Int])
    extends Flamingos2Controller[F] {
  private val L = Logger[F]

  override def observe(fileId:  ImageFileId,
                       expTime: Time): F[ObserveCommandResult] =
    sim.observe(fileId, expTime)

  override def applyConfig(config: Flamingos2Config): F[Unit] =
    L.info(s"Applying Flamingos-2 configuration $config") *>
      counter.modify(x => (x + 1, x + 1)) >>= {c => {
        counter.set(0) *>
        L.error(s"Error applying Flamingos-2 configuration") *>
        MonadError[F, Throwable].raiseError(Execution("simulated error"))
      }.whenA(c === failAt)}

  override def endObserve: F[Unit] = sim.endObserve

  override def observeProgress(total: Time): Stream[F, Progress] =
    sim.observeCountdown(total, ElapsedTime(0.seconds))
}

object Flamingos2ControllerSimBad {
  def apply[F[_]: Sync: Logger: Timer](failAt: Int): F[Flamingos2Controller[F]] =
    (Ref.of[F, Int](0), InstrumentControllerSim("FLAMINGOS-2 (bad)")).mapN { (counter, sim) =>
      Flamingos2ControllerSimBad(
        failAt,
        sim,
        counter
      )
    }

}
