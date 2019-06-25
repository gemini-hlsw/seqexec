// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import cats.effect.Sync
import seqexec.server.tcs.TcsController._
import cats.implicits._
import org.log4s.Logger


class TcsControllerSim[F[_]: Sync](logger: Logger) {

  def applyConfig(subsystems: NonEmptySet[Subsystem]): F[Unit] = {
    def configSubsystem(subsystem: Subsystem): F[Unit] =
      Sync[F].delay(logger.info(s"Applying ${subsystem.show} configuration."))

    subsystems.toList.map(configSubsystem).sequence.void
  }

  def notifyObserveStart: F[Unit] = Sync[F].delay(logger.info("Simulate TCS observe"))

  def notifyObserveEnd: F[Unit] = Sync[F].delay(logger.info("Simulate TCS endObserve"))
}
