// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.Applicative
import cats.effect.Sync
import org.typelevel.log4cats.Logger
import lucuma.core.enum.KeywordName
import seqexec.model.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._

object GcalHeader {
  implicit def header[F[_]: Sync: Logger](
    kwClient:   KeywordsClient[F],
    gcalReader: GcalKeywordReader[F]
  ): Header[F] =
    new Header[F] {
      private val gcalKeywords = List(
        buildString(gcalReader.lamp, KeywordName.GCALLAMP),
        buildString(gcalReader.filter, KeywordName.GCALFILT),
        buildString(gcalReader.diffuser, KeywordName.GCALDIFF),
        buildString(gcalReader.shutter, KeywordName.GCALSHUT)
      )

      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
        sendKeywords(id, kwClient, gcalKeywords)

      override def sendAfter(id: ImageFileId): F[Unit] = Applicative[F].unit
    }
}
