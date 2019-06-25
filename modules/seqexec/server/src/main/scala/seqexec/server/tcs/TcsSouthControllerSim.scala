// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import cats.effect.Sync
import org.log4s.getLogger
import seqexec.server.gems.Gems

class TcsSouthControllerSim[F[_]: Sync] private extends TcsSouthController[F] {
  val sim = new TcsControllerSim[F](TcsSouthControllerSim.Log)

  override def applyConfig(subsystems: NonEmptySet[TcsController.Subsystem],
                           gaos: Option[Gems[F]],
                           tc: TcsSouthController.TcsSouthConfig): F[Unit] =
    sim.applyConfig(subsystems)

  override def notifyObserveStart: F[Unit] = sim.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = sim.notifyObserveEnd
}

object TcsSouthControllerSim {
  val Log = getLogger

  def apply[F[_]: Sync]: TcsSouthController[F] = new TcsSouthControllerSim[F]
}
