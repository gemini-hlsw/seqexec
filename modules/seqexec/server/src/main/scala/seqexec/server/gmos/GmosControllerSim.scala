// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.effect.Sync
import cats.effect.Timer
import seqexec.model.dhs.ImageFileId
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.gmos.GmosController.{GmosConfig, NorthTypes, SiteDependentTypes, SouthTypes}
import seqexec.server.{InstrumentControllerSim, ObserveCommand, Progress}
import squants.Time

object GmosControllerSim {
  def apply[F[_]: Sync: Timer, T <: SiteDependentTypes](name: String): GmosController[F, T] =
    new GmosController[F, T] {
      private val sim: InstrumentControllerSim[F] = InstrumentControllerSim[F](s"GMOS $name")

      override def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommand.Result] =
        sim.observe(fileId, expTime)

      override def applyConfig(config: GmosConfig[T]): F[Unit] = sim.applyConfig(config)

      override def stopObserve: F[Unit] = sim.stopObserve

      override def abortObserve: F[Unit] = sim.abortObserve

      override def endObserve: F[Unit] = sim.endObserve

      override def pauseObserve: F[Unit] = sim.pauseObserve

      override def resumePaused(expTime: Time): F[ObserveCommand.Result] = sim.resumePaused

      override def stopPaused: F[ObserveCommand.Result] = sim.stopPaused

      override def abortPaused: F[ObserveCommand.Result] = sim.abortPaused

      override def observeProgress(total: Time, elapsed: ElapsedTime): fs2.Stream[F, Progress] =
        sim.observeCountdown(total, elapsed)

    }

  def south[F[_]: Sync: Timer]: GmosController[F, SouthTypes] = GmosControllerSim[F, SouthTypes]("South")
  def north[F[_]: Sync: Timer]: GmosController[F, NorthTypes] = GmosControllerSim[F, NorthTypes]("North")
}
