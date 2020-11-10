// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.Applicative
import cats.effect.Sync
import seqexec.model.Observation
import lucuma.core.enum.KeywordName
import io.chrisdavenport.log4cats.Logger
import seqexec.model.dhs.ImageFileId
import seqexec.server.InstrumentSystem
import seqexec.server.keywords._

object GcalHeader {
  implicit def header[F[_]: Sync: Logger](inst: InstrumentSystem[F], gcalReader: GcalKeywordReader[F]): Header[F] =
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
