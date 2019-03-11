// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.effect.Sync
import cats.implicits._
import edu.gemini.spModel.ao.AOConstants.AO_CONFIG_NAME
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.gemini.altair.AltairConstants.FIELD_LENSE_PROP
import seqexec.server.ConfigUtilOps._
import seqexec.model.enum.Resource
import seqexec.server.TrySeq
import seqexec.server.altair.AltairController._
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.tcs.Gaos.ResumeCondition
import seqexec.server.tcs.Gaos
import squants.Time

class Altair[F[_]: Sync] private (controller: AltairController[F],
                          fieldLens: FieldLens
                         ) extends Gaos[F] {
  override def pause(config: Either[AltairConfig, GemsConfig], reasons: Set[Gaos.PauseCondition]): F[Set[ResumeCondition] => F[Unit]] =
    config.swap.map(controller.pause(reasons, fieldLens)(_)).getOrElse({_:Set[ResumeCondition] => Sync[F].unit}.pure[F])

  override def observe(config: Either[AltairConfig, GemsConfig], expTime: Time): F[Unit] =
    config.swap.map(controller.observe(expTime)(_)).getOrElse(Sync[F].unit)

  override def endObserve(config: Either[AltairConfig, GemsConfig]): F[Unit] =
    config.swap.map(controller.endObserve).getOrElse(Sync[F].unit)

  val resource: Resource = Resource.Altair

  def usesP1(guide: AltairConfig): Boolean = guide match {
    case LgsWithP1 => true
    case _         => false
  }

  def usesOI(guide: AltairConfig): Boolean = guide match {
    case LgsWithOi |
         Ngs(true, _) => true
    case _            => false
  }

  def isFollowing: F[Option[Boolean]] = controller.isFollowing
}

object Altair {
  def fromConfig[F[_]: Sync](config: Config, controller: AltairController[F]): TrySeq[Altair[F]] =
    config.extractAs[FieldLens](new ItemKey(AO_CONFIG_NAME) / FIELD_LENSE_PROP).asTrySeq.map(new Altair(controller, _))
}