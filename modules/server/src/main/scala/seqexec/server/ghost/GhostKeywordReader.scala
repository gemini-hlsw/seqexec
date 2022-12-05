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
}

final class DefaultGhostKeywordsReader[F[_]: Applicative] extends GhostKeywordsReader[F] {
  val basePos: F[Boolean] = true.pure[F]
}

object GhostKeywordsReader extends GhostConfigUtil {
  def apply[F[_]: Sync](config: CleanConfig): GhostKeywordsReader[F] = {
    val raExtractor     = raExtractorBase(config)
    val decExtractor    = decExtractorBase(config)
    val defaultKeywords = new DefaultGhostKeywordsReader()

    (for {
      baseRAHMS  <- raExtractor(SPGhost.BASE_RA_HMS)
      baseDecDMS <- decExtractor(SPGhost.BASE_DEC_DMS)
    } yield new GhostKeywordsReader[F] {
      def basePos: F[Boolean] = (baseDecDMS.isEmpty && baseRAHMS.isEmpty).pure[F]
    }).getOrElse(defaultKeywords)
  }
}
