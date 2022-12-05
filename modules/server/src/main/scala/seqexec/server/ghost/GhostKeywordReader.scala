// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._
import seqexec.server.CleanConfig
import edu.gemini.spModel.gemini.ghost.{ Ghost => SPGhost }
import seqexec.server.ConfigUtilOps._
import java.lang.{ Boolean => JBoolean }

sealed trait GhostKeywordsReader[F[_]] {
  def basePos: F[Boolean]
  def srifu1: F[String]
  def srifu2: F[String]
  def hrifu1: F[String]
  def hrifu2: F[String]
  def fiberAgitator1Enabled: F[Boolean]
  def fiberAgitator2Enabled: F[Boolean]
}

final class DefaultGhostKeywordsReader[F[_]: Applicative] extends GhostKeywordsReader[F] {
  val basePos: F[Boolean]               = true.pure[F]
  def srifu1: F[String]                 = "".pure[F]
  def srifu2: F[String]                 = "".pure[F]
  def hrifu1: F[String]                 = "".pure[F]
  def hrifu2: F[String]                 = "".pure[F]
  val fiberAgitator1Enabled: F[Boolean] = false.pure[F]
  val fiberAgitator2Enabled: F[Boolean] = false.pure[F]
}

object GhostKeywordsReader extends GhostConfigUtil {
  def apply[F[_]: Sync](config: CleanConfig): GhostKeywordsReader[F] = {
    val raExtractor     = raExtractorBase(config)
    val decExtractor    = decExtractorBase(config)
    val defaultKeywords = new DefaultGhostKeywordsReader()

    (for {
      baseRAHMS     <- raExtractor(SPGhost.BASE_RA_HMS)
      baseDecDMS    <- decExtractor(SPGhost.BASE_DEC_DMS)
      srifu1Name     = extractor[String](config, SPGhost.SRIFU1_NAME)
      srifu2Name     = extractor[String](config, SPGhost.SRIFU2_NAME)
      hrifu1Name     = extractor[String](config, SPGhost.HRIFU1_NAME)
      hrifu2Name     = extractor[String](config, SPGhost.HRIFU2_NAME)
      srifu1Guiding  = extractor[String](config, SPGhost.SRIFU1_GUIDING)
      srifu2Guiding  = extractor[String](config, SPGhost.SRIFU2_GUIDING)
      hrifu1Guiding  = extractor[String](config, SPGhost.HRIFU1_GUIDING)
      fiberAgitator1 =
        config.extractInstAs[JBoolean](SPGhost.FIBER_AGITATOR_1).map(_.booleanValue())
      fiberAgitator2 =
        config.extractInstAs[JBoolean](SPGhost.FIBER_AGITATOR_2).map(_.booleanValue())
    } yield new GhostKeywordsReader[F] {
      println(srifu1Name)
      println(srifu2Name)
      println(hrifu1Name)
      println(hrifu2Name)
      def basePos: F[Boolean]               = (baseDecDMS.isEmpty && baseRAHMS.isEmpty).pure[F]
      def srifu1: F[String]                 = srifu1Name.getOrElse("    ").pure[F]
      def srifu2: F[String]                 = srifu2Name.getOrElse("    ").pure[F]
      def hrifu1: F[String]                 = hrifu1Name.getOrElse("    ").pure[F]
      def hrifu2: F[String]                 = hrifu2Name.getOrElse("    ").pure[F]
      val fiberAgitator1Enabled: F[Boolean] = fiberAgitator1.getOrElse(false).pure[F]
      val fiberAgitator2Enabled: F[Boolean] = fiberAgitator2.getOrElse(false).pure[F]
    }).getOrElse(defaultKeywords)
  }
}
