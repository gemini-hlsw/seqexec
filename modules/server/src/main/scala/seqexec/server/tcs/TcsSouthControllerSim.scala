// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.Applicative
import cats.data.NonEmptySet
import cats.implicits._
import org.typelevel.log4cats.Logger
import seqexec.model.enum.NodAndShuffleStage
import seqexec.server.gems.Gems
import seqexec.server.tcs.TcsController.InstrumentOffset
import seqexec.server.tcs.TcsController.Subsystem
import seqexec.server.tcs.TcsSouthController.{ TcsSouthConfig, tcsSouthConfigShow }

class TcsSouthControllerSim[F[_]: Applicative: Logger] private extends TcsSouthController[F] {
  val sim = new TcsControllerSim[F]
  val L   = Logger[F]

  override def applyConfig(
    subsystems: NonEmptySet[TcsController.Subsystem],
    gaos:       Option[Gems[F]],
    tc:         TcsSouthConfig
  ): F[Unit] =
    L.debug("Start TCS configuration") *>
      L.debug(s"TCS configuration: ${tc.show}") *>
      sim.applyConfig(subsystems) *>
      L.debug("Completed TCS configuration")

  override def notifyObserveStart: F[Unit] = sim.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = sim.notifyObserveEnd
  override def nod(
    subsystems: NonEmptySet[Subsystem],
    tcsConfig:  TcsSouthConfig
  )(stage:      NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean): F[Unit] =
    sim.nod(stage, offset, guided)
}

object TcsSouthControllerSim {

  def apply[F[_]: Applicative: Logger]: TcsSouthController[F] = new TcsSouthControllerSim[F]

}
