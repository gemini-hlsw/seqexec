// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.ApplicativeError
import cats.effect.Sync
import edu.gemini.spModel.gemini.altair.AltairConstants.GUIDESTAR_TYPE_PROP
import edu.gemini.spModel.gemini.altair.AltairParams.GuideStarType
import seqexec.model.`enum`.Instrument
import seqexec.model.enum.Resource
import seqexec.server.CleanConfig
import seqexec.server.ConfigUtilOps._
import seqexec.server.altair.AltairController._
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.tcs.Gaos
import seqexec.server.tcs.Gaos.{ PauseConditionSet, ResumeConditionSet }
import seqexec.server.tcs.TcsController.FocalPlaneOffset
import squants.Time

trait Altair[F[_]] extends Gaos[F] {

  def pauseResume(
    config:        AltairConfig,
    currOffset:    FocalPlaneOffset,
    instrument:    Instrument,
    pauseReasons:  PauseConditionSet,
    resumeReasons: ResumeConditionSet
  ): F[AltairPauseResume[F]]

  val resource: Resource

  def usesP1(guide: AltairConfig): Boolean

  def usesOI(guide: AltairConfig): Boolean

  def isFollowing: F[Boolean]

  // Are we using a NGS, either for NGS mode or LGS + NGS ?
  def hasTarget(guide: AltairConfig): Boolean

}

object Altair {

  private class AltairImpl[F[_]: Sync](controller: AltairController[F]) extends Altair[F] {
    override def pauseResume(
      config:        AltairConfig,
      currOffset:    FocalPlaneOffset,
      instrument:    Instrument,
      pauseReasons:  PauseConditionSet,
      resumeReasons: ResumeConditionSet
    ): F[AltairPauseResume[F]] =
      controller.pauseResume(pauseReasons, resumeReasons, currOffset, instrument)(config)

    override def observe(config: Either[AltairConfig, GemsConfig], expTime: Time): F[Unit] =
      config.swap.map(controller.observe(expTime)(_)).getOrElse(Sync[F].unit)

    override def endObserve(config: Either[AltairConfig, GemsConfig]): F[Unit] =
      config.swap.map(controller.endObserve).getOrElse(Sync[F].unit)

    override val resource: Resource = Resource.Altair

    override def usesP1(guide: AltairConfig): Boolean = guide match {
      case LgsWithP1 => true
      case _         => false
    }

    override def usesOI(guide: AltairConfig): Boolean = guide match {
      case LgsWithOi | Ngs(true, _) => true
      case _                        => false
    }

    override def isFollowing: F[Boolean] = controller.isFollowing

    override def hasTarget(guide: AltairConfig): Boolean = guide match {
      case Lgs(st, sf, _) => st || sf
      case LgsWithOi      => false
      case LgsWithP1      => false
      case Ngs(_, _)      => true
      case AltairOff      => false
    }

  }

  def apply[F[_]: Sync](controller: AltairController[F]): Altair[F] = new AltairImpl[F](controller)

  def guideStarType[F[_]: ApplicativeError[*[_], Throwable]](
    config: CleanConfig
  ): F[GuideStarType] =
    config.extractAOAs[GuideStarType](GUIDESTAR_TYPE_PROP).toF[F]

}
