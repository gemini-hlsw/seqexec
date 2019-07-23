// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data._
import cats.effect.IO
import cats.implicits._
import seqexec.server.altair.Altair
import seqexec.server.tcs.TcsController._
import seqexec.server.SeqexecFailure
import seqexec.server.tcs.TcsNorthController.{TcsNorthAoConfig, TcsNorthConfig}

class TcsNorthControllerEpics private extends TcsNorthController[IO] {

  override def applyConfig(subsystems: NonEmptySet[Subsystem],
                           gaos: Option[Altair[IO]],
                           tcs: TcsNorthConfig): IO[Unit] = {
    tcs match {
      case c: BasicTcsConfig   => TcsControllerEpicsCommon.applyBasicConfig(subsystems, c)
      case d: TcsNorthAoConfig => gaos.map(TcsNorthControllerEpicsAo.applyAoConfig(subsystems, _, d))
        .getOrElse(SeqexecFailure.Execution("No Altair object defined for Altair step").raiseError[IO, Unit])
    }
  }

  override def notifyObserveStart: IO[Unit] = TcsControllerEpicsCommon.notifyObserveStart

  override def notifyObserveEnd: IO[Unit] = TcsControllerEpicsCommon.notifyObserveEnd
}

object TcsNorthControllerEpics {

  def apply(): TcsNorthController[IO] = new TcsNorthControllerEpics

}
