// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
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
        buildString[F](gcalReader.getLamp.orDefault, KeywordName.GCALLAMP),
        buildString[F](gcalReader.getFilter.orDefault, KeywordName.GCALFILT),
        buildString[F](gcalReader.getDiffuser.orDefault, KeywordName.GCALDIFF),
        buildString[F](gcalReader.getShutter.orDefault, KeywordName.GCALSHUT)
      )

      override def sendBefore(obsId: Observation.Id,
                              id: ImageFileId): F[Unit] =
        sendKeywords(id, inst, gcalKeywords)

      override def sendAfter(id: ImageFileId): F[Unit] = Applicative[F].unit
    }
}
