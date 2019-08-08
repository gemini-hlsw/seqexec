// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Applicative
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.gems.Gems.GemsWfsStatus
import seqexec.server.gems.GemsController.{GemsConfig, GemsOff}
import seqexec.server.tcs.Gaos
import seqexec.server.tcs.Gaos.{PauseConditionSet, PauseResume, ResumeConditionSet}
import squants.Time

trait Gems[F[_]] extends Gaos[F] {
  val cfg: GemsConfig

  def pauseResume(config: GemsConfig, pauseReasons: PauseConditionSet,
                  resumeReasons: ResumeConditionSet): F[PauseResume[F]]

  val stateGetter: GemsWfsStatus[F]

}

object Gems {

  private class GemsImpl[F[_]: Applicative] (controller: GemsController[F], config: GemsConfig) extends Gems[F] {

    override val cfg: GemsConfig = config

    override def observe(config: Either[AltairConfig, GemsConfig], expTime: Time): F[Unit] = controller.observe(expTime)

    override def endObserve(config: Either[AltairConfig, GemsConfig]): F[Unit] = controller.endObserve

    override def pauseResume(config: GemsConfig,
                             pauseReasons: PauseConditionSet,
                             resumeReasons: ResumeConditionSet): F[PauseResume[F]] = {
      controller.pauseResume(cfg, pauseReasons, resumeReasons)
    }

    override val stateGetter: GemsWfsStatus[F] = controller.stateGetter
  }

  def fromConfig[F[_]: Applicative](controller: GemsController[F])/*(config: Config)*/: F[Gems[F]] =
    Applicative[F].pure(new GemsImpl[F](controller, GemsOff))

  final case class GemsWfsStatus[F[_]](
    ngs1: F[Option[Boolean]],
    ngs2: F[Option[Boolean]],
    ngs3: F[Option[Boolean]],
    odgw1: F[Option[Boolean]],
    odgw2: F[Option[Boolean]],
    odgw3: F[Option[Boolean]],
    odgw4: F[Option[Boolean]]
  )

}




