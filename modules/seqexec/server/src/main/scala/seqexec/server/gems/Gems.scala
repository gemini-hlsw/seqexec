// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Applicative
import cats.implicits._
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.gems.GemsController.{GemsConfig, GemsOff}
import seqexec.server.tcs.Gaos
import seqexec.server.tcs.Gaos.ResumeCondition
import squants.Time

class Gems[F[_]: Applicative] private (controller: GemsController[F], config: GemsConfig) extends Gaos[F] {

  val cfg = config

  override def pause(config: Either[AltairConfig, GemsConfig], reasons: Set[Gaos.PauseCondition]): F[Set[ResumeCondition] => F[Unit]] =
    {_:Set[ResumeCondition] => ().pure[F]}.pure[F]

  override def observe(config: Either[AltairConfig, GemsConfig], expTime: Time): F[Unit] = controller.observe(expTime)

  override def endObserve(config: Either[AltairConfig, GemsConfig]): F[Unit] = controller.endObserve

}

object Gems {
  def fromConfig[F[_]: Applicative](controller: GemsController[F])/*(config: Config)*/: F[Gems[F]] =
    Applicative[F].pure(new Gems[F](controller, GemsOff))
}




