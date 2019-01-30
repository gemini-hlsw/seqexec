// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.Monad
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.{InstrumentSystem, SeqActionF}

object GcalHeader {
  implicit def header[F[_]: Monad](inst: InstrumentSystem[F], gcalReader: GcalKeywordReader[F]): Header[F] =
    new Header[F] {
      private val gcalKeywords = List(
        buildString[F](gcalReader.getLamp.orDefault, KeywordName.GCALLAMP),
        buildString[F](gcalReader.getFilter.orDefault, KeywordName.GCALFILT),
        buildString[F](gcalReader.getDiffuser.orDefault, KeywordName.GCALDIFF),
        buildString[F](gcalReader.getShutter.orDefault, KeywordName.GCALSHUT)
      )

      override def sendBefore(obsId: Observation.Id,
                              id: ImageFileId): SeqActionF[F, Unit] =
        sendKeywords(id, inst, gcalKeywords)

      override def sendAfter(id: ImageFileId): SeqActionF[F, Unit] = SeqActionF.void[F]
    }
}
