// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.Applicative
import cats.effect.Sync
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.InstrumentSystem

object GcalHeader {
  implicit def header[F[_]: Sync](inst: InstrumentSystem[F], gcalReader: GcalKeywordReader[F]): Header[F] =
    new Header[F] {
      private val gcalKeywords = List(
        buildString(gcalReader.lamp, KeywordName.GCALLAMP),
        buildString(gcalReader.filter, KeywordName.GCALFILT),
        buildString(gcalReader.diffuser, KeywordName.GCALDIFF),
        buildString(gcalReader.shutter, KeywordName.GCALSHUT)
      )

      override def sendBefore(obsId: Observation.Id,
                              id: ImageFileId): F[Unit] =
        sendKeywords(id, inst, gcalKeywords)

      override def sendAfter(id: ImageFileId): F[Unit] = Applicative[F].unit
    }
}
