// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.{EitherT, NonEmptySet}
import cats.effect.IO
import org.log4s.getLogger
import seqexec.server.tcs.TcsController._
import seqexec.server.SeqAction
import cats.implicits._
import seqexec.server.altair.Altair
import seqexec.server.gems.Gems

object TcsControllerSim extends TcsController {

  private val Log = getLogger

  override def applyConfig(subsystems: NonEmptySet[Subsystem],
                           gaos: Option[Either[Altair[IO], Gems[IO]]],
                           tc: TcsConfig): SeqAction[Unit] = {
    def configSubsystem(subsystem: Subsystem): IO[Unit] = IO.apply(Log.info(s"Applying ${subsystem.show} configuration."))

    SeqAction.lift(subsystems.toList.map(configSubsystem).sequence.as(()))
  }

  override def notifyObserveStart: SeqAction[Unit] = EitherT.right(IO(Log.info("Simulate TCS observe")))

  override def notifyObserveEnd: SeqAction[Unit] = EitherT.right(IO(Log.info("Simulate TCS endObserve")))
}
