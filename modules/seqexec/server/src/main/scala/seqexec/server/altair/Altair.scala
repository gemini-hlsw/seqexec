// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.Monad
import cats.effect.Sync
import cats.implicits._
import seqexec.model.enum.Resource
import seqexec.server.altair.AltairController._
import seqexec.server.tcs.{Gaos, GuideConfigDb}
import squants.Time

class Altair[F[_]: Sync] (controller: AltairController[F],
                                  cfgDb: GuideConfigDb[F],
                                  fieldLens: FieldLens
                                 ) extends Gaos[F] {
  override def pause(reasons: Set[Gaos.PauseReason]): F[Unit] =
    withSavedConfig(controller.pause(reasons, fieldLens))

  override def resume(reasons: Set[Gaos.ResumeReason]): F[Unit] =
    withSavedConfig(controller.resume(reasons))

  override def observe(expTime: Time): F[Unit] =
    withSavedConfig(controller.observe(expTime))

  override def endObserve: F[Unit] =
    withSavedConfig(controller.endObserve)

  val resource: Resource = Resource.Altair

  private def withSavedConfig(f: AltairConfig => F[Unit]): F[Unit] =
    Monad[F].flatMap(cfgDb.value){ cfg =>
      cfg.gaosGuide.flatMap(_.swap.toOption.map(f))
        .getOrElse(Sync[F].unit)
    }

  val usesP1: F[Boolean] = cfgDb.value.map{ _.gaosGuide match {
    case Some(Left(LgsWithP1)) => true
    case _                     => false
    }
  }

  val usesOI: F[Boolean] = cfgDb.value.map {
    _.gaosGuide match {
      case Some(Left(LgsWithOi)) |
           Some(Left(Ngs(true))) => true
      case _ => false
    }
  }
}