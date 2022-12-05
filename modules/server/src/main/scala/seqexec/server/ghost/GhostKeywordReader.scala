// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._
import seqexec.server.CleanConfig
import edu.gemini.spModel.gemini.ghost.{ Ghost => SPGhost }

sealed trait GhostKeywordsReader[F[_]] {
  def basePos: F[Boolean]
  def srifu1: F[String]
  def srifu2: F[String]
  def hrifu1: F[String]
  def hrifu2: F[String]
}

final class DefaultGhostKeywordsReader[F[_]: Applicative] extends GhostKeywordsReader[F] {
  val basePos: F[Boolean] = true.pure[F]
  def srifu1: F[String]   = "".pure[F]
  def srifu2: F[String]   = "".pure[F]
  def hrifu1: F[String]   = "".pure[F]
  def hrifu2: F[String]   = "".pure[F]
}

object GhostKeywordsReader extends GhostConfigUtil {
  def apply[F[_]: Sync](config: CleanConfig): GhostKeywordsReader[F] = {
    val raExtractor     = raExtractorBase(config)
    val decExtractor    = decExtractorBase(config)
    val defaultKeywords = new DefaultGhostKeywordsReader()

    (for {
      baseRAHMS  <- raExtractor(SPGhost.BASE_RA_HMS)
      baseDecDMS <- decExtractor(SPGhost.BASE_DEC_DMS)
      srifu1Name  = extractor[String](config, SPGhost.SRIFU1_NAME)
      srifu2Name  = extractor[String](config, SPGhost.SRIFU2_NAME)
      hrifu1Name  = extractor[String](config, SPGhost.HRIFU1_NAME)
      hrifu2Name  = extractor[String](config, SPGhost.HRIFU2_NAME)
    } yield new GhostKeywordsReader[F] {
      println(srifu1Name)
      println(srifu2Name)
      println(hrifu1Name)
      println(hrifu2Name)
      def basePos: F[Boolean] = (baseDecDMS.isEmpty && baseRAHMS.isEmpty).pure[F]
      def srifu1: F[String]   = srifu1Name.orEmpty.pure[F]
      def srifu2: F[String]   = srifu2Name.orEmpty.pure[F]
      def hrifu1: F[String]   = hrifu1Name.orEmpty.pure[F]
      def hrifu2: F[String]   = hrifu2Name.orEmpty.pure[F]
    }).getOrElse(defaultKeywords)
  }
}
