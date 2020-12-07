// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data._
import cats.effect.Async
import cats.effect.Timer
import cats.syntax.all._
import io.chrisdavenport.log4cats.Logger
import seqexec.model.enum.NodAndShuffleStage
import seqexec.server.SeqexecFailure
import seqexec.server.altair.Altair
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsNorthController.TcsNorthAoConfig
import seqexec.server.tcs.TcsNorthController.TcsNorthConfig

final case class TcsNorthControllerEpics[F[_]: Async: Logger: Timer](epicsSys: TcsEpics[F]) extends TcsNorthController[F] {
  private val commonController = TcsControllerEpicsCommon(epicsSys)
  private val aoController = TcsNorthControllerEpicsAo(epicsSys)

  override def applyConfig(subsystems: NonEmptySet[Subsystem],
                           gaos: Option[Altair[F]],
                           tcs: TcsNorthConfig): F[Unit] = {
    tcs match {
      case c: BasicTcsConfig   => commonController.applyBasicConfig(subsystems, c)
      case d: TcsNorthAoConfig => gaos.map(aoController.applyAoConfig(subsystems, _, d))
        .getOrElse(SeqexecFailure.Execution("No Altair object defined for Altair step").raiseError[F, Unit])
    }
  }

  override def notifyObserveStart: F[Unit] = commonController.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = commonController.notifyObserveEnd

  override def nod(subsystems: NonEmptySet[Subsystem], tcsConfig: TcsNorthConfig)
                  (stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean)
  : F[Unit] = tcsConfig match {
    case c: BasicTcsConfig => commonController.nod(subsystems, offset, guided, c)
    case _: TcsNorthAoConfig => SeqexecFailure.Execution("N&S not supported when using Altair").raiseError[F, Unit]
  }
}
