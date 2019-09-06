// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data._
import cats.effect.IO
import cats.implicits._
import seqexec.model.enum.NodAndShuffleStage
import seqexec.server.SeqexecFailure
import seqexec.server.gems.Gems
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsSouthController._

class TcsSouthControllerEpics private (guideConfigDb: GuideConfigDb[IO]) extends TcsSouthController[IO] {
  override def applyConfig(subsystems: NonEmptySet[Subsystem],
                           gaos: Option[Gems[IO]],
                           tcs: TcsSouthConfig): IO[Unit] = {
    tcs match {
      case c: BasicTcsConfig   => TcsControllerEpicsCommon.applyBasicConfig(subsystems, c)
      case d: TcsSouthAoConfig => for {
        oc <- guideConfigDb.value
        gc <- oc.gaosGuide.flatMap(_.toOption).map(_.pure[IO])
          .getOrElse(SeqexecFailure.Execution("Attemp to run GeMS step before the operator configured GeMS").raiseError[IO, GemsConfig])
        ob <- gaos.map(_.pure[IO])
          .getOrElse(SeqexecFailure.Execution("No GeMS object defined for GeMS step").raiseError[IO, Gems[IO]])
        r  <- TcsSouthControllerEpicsAo.applyAoConfig(subsystems, ob, gc, d)
      } yield r
    }
  }

  override def notifyObserveStart: IO[Unit] = TcsControllerEpicsCommon.notifyObserveStart

  override def notifyObserveEnd: IO[Unit] = TcsControllerEpicsCommon.notifyObserveEnd

  override def nod(subsystems: NonEmptySet[Subsystem], tcsConfig: TcsSouthConfig)
                  (stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean)
  : IO[Unit] = tcsConfig match {
    case c: BasicTcsConfig => TcsControllerEpicsCommon.nod(subsystems, offset, guided, c)
    case _: TcsSouthAoConfig => SeqexecFailure.Execution("N&S not supported when using GeMS").raiseError[IO, Unit]
  }
}

object TcsSouthControllerEpics {

  def apply(guideConfigDb: GuideConfigDb[IO]): TcsSouthController[IO] = new TcsSouthControllerEpics(guideConfigDb)

}
