// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Applicative
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.gems.Gems.GemsGuiderStatus
import seqexec.server.gems.GemsController.{GemsConfig, GemsOff, GemsOn}
import seqexec.server.tcs.Gaos
import seqexec.server.tcs.Gaos.{PauseResume, ResumeCondition}
import squants.Time

trait Gems[F[_]] extends Gaos[F] {
  val cfg: GemsConfig

  def pauseResume(config: GemsConfig, pauseReasons: Set[Gaos.PauseCondition],
                  resumeReasons: Set[ResumeCondition]): F[PauseResume[F]]

  def usesP1(guide: GemsConfig): Boolean

  def usesOI(guide: GemsConfig): Boolean

  val stateGetter: GemsGuiderStatus[F]

}

object Gems {

  private class GemsImpl[F[_]: Applicative] (controller: GemsController[F], config: GemsConfig) extends Gems[F] {

    override val cfg: GemsConfig = config

    override def observe(config: Either[AltairConfig, GemsConfig], expTime: Time): F[Unit] = controller.observe(expTime)

    override def endObserve(config: Either[AltairConfig, GemsConfig]): F[Unit] = controller.endObserve

    override def pauseResume(config: GemsConfig,
                             pauseReasons: Set[Gaos.PauseCondition],
                             resumeReasons: Set[ResumeCondition]): F[PauseResume[F]] = {
      controller.pauseResume(cfg, pauseReasons, resumeReasons)
    }

    override def usesP1(guide: GemsConfig): Boolean = guide match {
      case GemsOn(_, _, _, _, _, _, _, _, useP1) => useP1
      case _                                     => false
    }

    override def usesOI(guide: GemsConfig): Boolean = guide match {
      case GemsOn(_, _, _, _, _, _, _, useOI, _) => useOI
      case _                                     => false
    }

    override val stateGetter: GemsGuiderStatus[F] = controller.stateGetter
  }

  def fromConfig[F[_]: Applicative](controller: GemsController[F])/*(config: Config)*/: F[Gems[F]] =
    Applicative[F].pure(new GemsImpl[F](controller, GemsOff))

  final case class GemsGuiderStatus[F[_]](
    ngs1: F[Option[Boolean]],
    ngs2: F[Option[Boolean]],
    ngs3: F[Option[Boolean]],
    odgw1: F[Option[Boolean]],
    odgw2: F[Option[Boolean]],
    odgw3: F[Option[Boolean]],
    odgw4: F[Option[Boolean]]
  )

}




