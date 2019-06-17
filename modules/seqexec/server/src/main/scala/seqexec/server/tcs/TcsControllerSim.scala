// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import cats.effect.Sync
import org.log4s.getLogger
import seqexec.server.tcs.TcsController._
import cats.implicits._
import seqexec.server.altair.Altair
import seqexec.server.gems.Gems


class TcsControllerSim[F[_]: Sync] private extends TcsController[F] {

  private val Log = getLogger

  override def applyConfig(subsystems: NonEmptySet[Subsystem],
                           gaos: Option[Either[Altair[F], Gems[F]]],
                           tc: TcsConfig): F[Unit] = {
    def configSubsystem(subsystem: Subsystem): F[Unit] =
      Sync[F].delay(Log.info(s"Applying ${subsystem.show} configuration."))

    subsystems.toList.map(configSubsystem).sequence.void
  }

  override def notifyObserveStart: F[Unit] = Sync[F].delay(Log.info("Simulate TCS observe"))

  override def notifyObserveEnd: F[Unit] = Sync[F].delay(Log.info("Simulate TCS endObserve"))
}

object TcsControllerSim {

  def apply[F[_]: Sync]: TcsController[F] = new TcsControllerSim[F]

}
