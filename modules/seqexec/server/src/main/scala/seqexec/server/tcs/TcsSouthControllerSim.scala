// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import cats.Applicative
import io.chrisdavenport.log4cats.Logger
import seqexec.model.enum.NodAndShuffleStage
import seqexec.server.gems.Gems
import seqexec.server.tcs.TcsController.{InstrumentOffset, Subsystem}
import seqexec.server.tcs.TcsSouthController.TcsSouthConfig

class TcsSouthControllerSim[F[_]: Applicative: Logger] private extends TcsSouthController[F] {
  val sim = new TcsControllerSim[F]

  override def applyConfig(subsystems: NonEmptySet[TcsController.Subsystem],
                           gaos: Option[Gems[F]],
                           tc: TcsSouthConfig): F[Unit] =
    sim.applyConfig(subsystems)

  override def notifyObserveStart: F[Unit] = sim.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = sim.notifyObserveEnd
  override def nod(subsystems: NonEmptySet[Subsystem], tcsConfig: TcsSouthConfig)
                  (stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean)
  : F[Unit] = sim.nod(stage, offset, guided)
}

object TcsSouthControllerSim {

  def apply[F[_]: Applicative: Logger]: TcsSouthController[F] = new TcsSouthControllerSim[F]

}
