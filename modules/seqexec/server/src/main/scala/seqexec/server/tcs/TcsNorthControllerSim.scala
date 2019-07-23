// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import cats.effect.Sync
import seqexec.server.altair.Altair

class TcsNorthControllerSim[F[_]: Sync] private extends TcsNorthController[F] {
 val sim = new TcsControllerSim[F]

  override def applyConfig(subsystems: NonEmptySet[TcsController.Subsystem],
                           gaos: Option[Altair[F]],
                           tc: TcsNorthController.TcsNorthConfig): F[Unit] =
    sim.applyConfig(subsystems)

  override def notifyObserveStart: F[Unit] = sim.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = sim.notifyObserveEnd
}

object TcsNorthControllerSim {

  def apply[F[_]: Sync]: TcsNorthController[F] = new TcsNorthControllerSim[F]

}
