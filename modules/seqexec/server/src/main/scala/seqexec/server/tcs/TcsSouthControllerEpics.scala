// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data._
import cats.effect.IO
import cats.implicits._
import seqexec.server.SeqexecFailure
import seqexec.server.gems.Gems
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsSouthController._

class TcsSouthControllerEpics private extends TcsSouthController[IO] {
  override def applyConfig(subsystems: NonEmptySet[Subsystem],
                           gaos: Option[Gems[IO]],
                           tcs: TcsSouthConfig): IO[Unit] = {
    tcs match {
      case c: BasicTcsConfig => TcsControllerEpicsCommon.applyBasicConfig(subsystems, c)
      case _ => SeqexecFailure.Execution("GeMS steps not yet supported").raiseError[IO, Unit]
    }
  }

  override def notifyObserveStart: IO[Unit] = TcsControllerEpicsCommon.notifyObserveStart

  override def notifyObserveEnd: IO[Unit] = TcsControllerEpicsCommon.notifyObserveEnd
}

object TcsSouthControllerEpics {

  def apply(): TcsSouthController[IO] = new TcsSouthControllerEpics

}
