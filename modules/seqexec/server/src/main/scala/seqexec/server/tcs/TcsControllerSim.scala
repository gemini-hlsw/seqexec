// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.Applicative
import cats.data.NonEmptySet
import org.log4s.getLogger
import seqexec.server.tcs.TcsController._
import cats.implicits._
import seqexec.server.altair.Altair
import seqexec.server.gems.Gems


class TcsControllerSim[F[_]: Applicative] private extends TcsController[F] {

  private val Log = getLogger

  override def applyConfig(subsystems: NonEmptySet[Subsystem],
                           gaos: Option[Either[Altair[F], Gems[F]]],
                           tc: TcsConfig): F[Unit] = {
    def configSubsystem(subsystem: Subsystem): F[Unit] = Log.info(s"Applying ${subsystem.show} configuration.").pure[F]

    subsystems.toList.map(configSubsystem).sequence.void
  }

  override def notifyObserveStart: F[Unit] = Log.info("Simulate TCS observe").pure[F]

  override def notifyObserveEnd: F[Unit] = Log.info("Simulate TCS endObserve").pure[F]
}

object TcsControllerSim {

  def apply[F[_]: Applicative]: TcsController[F] = new TcsControllerSim[F]

}
