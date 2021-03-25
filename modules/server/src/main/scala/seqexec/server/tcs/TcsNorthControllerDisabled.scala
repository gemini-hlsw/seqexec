// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import org.typelevel.log4cats.Logger
import seqexec.model.`enum`.NodAndShuffleStage
import seqexec.server.overrideLogMessage
import seqexec.server.altair.Altair
import seqexec.server.tcs.TcsNorthController.TcsNorthConfig

class TcsNorthControllerDisabled[F[_]: Logger] extends TcsNorthController[F] {
  override def applyConfig(
    subsystems: NonEmptySet[TcsController.Subsystem],
    gaos:       Option[Altair[F]],
    tc:         TcsNorthConfig
  ): F[Unit] =
    overrideLogMessage("TCS", "applyConfig")

  override def notifyObserveStart: F[Unit] = overrideLogMessage("TCS", "notifyObserveStart")

  override def notifyObserveEnd: F[Unit] = overrideLogMessage("TCS", "notifyObserveEnd")

  override def nod(
    subsystems: NonEmptySet[TcsController.Subsystem],
    tcsConfig:  TcsNorthConfig
  )(stage:      NodAndShuffleStage, offset: TcsController.InstrumentOffset, guided: Boolean): F[Unit] =
    overrideLogMessage("TCS", s"nod(${stage.symbol})")
}
