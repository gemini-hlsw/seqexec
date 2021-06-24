// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import cats.implicits._
import org.typelevel.log4cats.Logger
import seqexec.model.enum.NodAndShuffleStage
import seqexec.server.tcs.TcsController._

class TcsControllerSim[F[_]: Logger] {

  def info(msg: String): F[Unit] = Logger[F].info(msg)

  def applyConfig(subsystems: NonEmptySet[Subsystem]): F[Unit] =
    info(s"Simulate TCS configuration for ${subsystems.toList.mkString("(", ", ", ")")}")

  def notifyObserveStart: F[Unit] = info("Simulate TCS observe")

  def notifyObserveEnd: F[Unit] = info("Simulate TCS endObserve")

  def nod(stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean): F[Unit] =
    info(s"Simulate TCS Nod to position ${stage.symbol}, offset=$offset, guided=$guided")
}
