// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Applicative
import seqexec.server.gems.GemsController.{GemsConfig, GemsOff}
import seqexec.server.tcs.Gaos
import squants.Time

class Gems[F[_]: Applicative] private (controller: GemsController[F], config: GemsConfig) extends Gaos[F] {
  override def pause(reasons: Set[Gaos.PauseReason]): F[Unit] = controller.pause(reasons)

  override def resume(reasons: Set[Gaos.ResumeReason]): F[Unit] = controller.resume(reasons, config)

  override def observe(expTime: Time): F[Unit] = controller.observe(expTime)

  override def endObserve: F[Unit] = controller.endObserve
}

object Gems {
  def fromConfig[F[_]: Applicative](controller: GemsController[F])/*(config: Config)*/: F[Gems[F]] =
    Applicative[F].pure(new Gems[F](controller, GemsOff))
}




