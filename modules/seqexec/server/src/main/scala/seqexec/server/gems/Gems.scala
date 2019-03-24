// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Applicative
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.gems.GemsController.{GemsConfig, GemsOff}
import seqexec.server.tcs.Gaos
import squants.Time

class Gems[F[_]: Applicative] private (controller: GemsController[F], config: GemsConfig) extends Gaos[F] {

  val cfg = config

  override def observe(config: Either[AltairConfig, GemsConfig], expTime: Time): F[Unit] = controller.observe(expTime)

  override def endObserve(config: Either[AltairConfig, GemsConfig]): F[Unit] = controller.endObserve

}

object Gems {
  def fromConfig[F[_]: Applicative](controller: GemsController[F])/*(config: Config)*/: F[Gems[F]] =
    Applicative[F].pure(new Gems[F](controller, GemsOff))
}




