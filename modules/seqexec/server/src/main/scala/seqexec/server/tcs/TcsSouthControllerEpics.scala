// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data._
import cats.effect._
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import seqexec.model.enum.NodAndShuffleStage
import seqexec.server.SeqexecFailure
import seqexec.server.gems.Gems
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsSouthController._

final case class TcsSouthControllerEpics[F[_]: Async: Logger: Timer](epicsSys: TcsEpics[F], guideConfigDb: GuideConfigDb[F]) extends TcsSouthController[F] {
  private val commonController = TcsControllerEpicsCommon(epicsSys)
  private val aoController = TcsSouthControllerEpicsAo(epicsSys)

  override def applyConfig(subsystems: NonEmptySet[Subsystem],
                           gaos: Option[Gems[F]],
                           tcs: TcsSouthConfig): F[Unit] = {
    tcs match {
      case c: BasicTcsConfig   => commonController.applyBasicConfig(subsystems, c)
      case d: TcsSouthAoConfig => for {
        oc <- guideConfigDb.value
        gc <- oc.gaosGuide.flatMap(_.toOption).map(_.pure[F])
          .getOrElse(SeqexecFailure.Execution("Attemp to run GeMS step before the operator configured GeMS").raiseError[F, GemsConfig])
        ob <- gaos.map(_.pure[F])
          .getOrElse(SeqexecFailure.Execution("No GeMS object defined for GeMS step").raiseError[F, Gems[F]])
        r  <- aoController.applyAoConfig(subsystems, ob, gc, d)
      } yield r
    }
  }

  override def notifyObserveStart: F[Unit] = commonController.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = commonController.notifyObserveEnd

  override def nod(subsystems: NonEmptySet[Subsystem], tcsConfig: TcsSouthConfig)
                  (stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean)
  : F[Unit] = tcsConfig match {
    case c: BasicTcsConfig => commonController.nod(subsystems, offset, guided, c)
    case _: TcsSouthAoConfig => SeqexecFailure.Execution("N&S not supported when using GeMS").raiseError[F, Unit]
  }
}
