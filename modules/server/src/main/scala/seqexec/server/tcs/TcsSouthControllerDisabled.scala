// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import io.chrisdavenport.log4cats.Logger
import seqexec.model.`enum`.NodAndShuffleStage
import seqexec.server.overrideLogMessage
import seqexec.server.gems.Gems
import seqexec.server.tcs.TcsSouthController.TcsSouthConfig

class TcsSouthControllerDisabled[F[_]: Logger] extends TcsSouthController[F] {
  override def applyConfig(
    subsystems: NonEmptySet[TcsController.Subsystem],
    gaos:       Option[Gems[F]],
    tc:         TcsSouthConfig
  ): F[Unit] =
    overrideLogMessage("TCS", "applyConfig")

  override def notifyObserveStart: F[Unit] = overrideLogMessage("TCS", "notifyObserveStart")

  override def notifyObserveEnd: F[Unit] = overrideLogMessage("TCS", "notifyObserveEnd")

  override def nod(
    subsystems: NonEmptySet[TcsController.Subsystem],
    tcsConfig:  TcsSouthConfig
  )(stage:      NodAndShuffleStage, offset: TcsController.InstrumentOffset, guided: Boolean): F[Unit] =
    overrideLogMessage("TCS", s"nod(${stage.symbol})")
}
