// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Applicative
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.gems.GemsController.{GemsConfig, GemsOff}
import seqexec.server.tcs.Gaos
import seqexec.server.tcs.Gaos.{PauseResume, ResumeCondition}
import squants.Time

trait Gems[F[_]] extends Gaos[F] {
  val cfg: GemsConfig

  def pauseResume(config: GemsConfig, pauseReasons: Set[Gaos.PauseCondition],
                  resumeReasons: Set[ResumeCondition]): F[PauseResume[F]]
}


object Gems {

  private class GemsImpl[F[_]: Applicative] (controller: GemsController[F], config: GemsConfig) extends Gems[F] {

    override val cfg: GemsConfig = config

    override def observe(config: Either[AltairConfig, GemsConfig], expTime: Time): F[Unit] = controller.observe(expTime)

    override def endObserve(config: Either[AltairConfig, GemsConfig]): F[Unit] = controller.endObserve

    override def pauseResume(config: GemsConfig,
                             pauseReasons: Set[Gaos.PauseCondition],
                             resumeReasons: Set[ResumeCondition]): F[PauseResume[F]] = {
      Applicative[F].pure(PauseResume(None, None))
    }
  }

  def fromConfig[F[_]: Applicative](controller: GemsController[F])/*(config: Config)*/: F[Gems[F]] =
    Applicative[F].pure(new GemsImpl[F](controller, GemsOff))
}




