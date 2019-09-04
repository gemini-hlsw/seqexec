// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import cats.effect.Sync
import seqexec.model.enum.NodAndShuffleStage
import seqexec.server.altair.Altair
import seqexec.server.tcs.TcsController.{InstrumentOffset, Subsystem}
import seqexec.server.tcs.TcsNorthController.TcsNorthConfig

class TcsNorthControllerSim[F[_]: Sync] private extends TcsNorthController[F] {
 val sim = new TcsControllerSim[F]

  override def applyConfig(subsystems: NonEmptySet[Subsystem],
                           gaos: Option[Altair[F]],
                           tc: TcsNorthConfig): F[Unit] =
    sim.applyConfig(subsystems)

  override def notifyObserveStart: F[Unit] = sim.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = sim.notifyObserveEnd

  override def nod(subsystems: NonEmptySet[Subsystem], tcsConfig: TcsNorthConfig)
                  (stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean)
  : F[Unit] = sim.nod(stage, offset, guided)

}

object TcsNorthControllerSim {

  def apply[F[_]: Sync]: TcsNorthController[F] = new TcsNorthControllerSim[F]

}
