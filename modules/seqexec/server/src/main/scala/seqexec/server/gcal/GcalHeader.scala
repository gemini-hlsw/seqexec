// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.{InstrumentSystem, SeqAction}

object GcalHeader {
  implicit def header[F[_]](inst: InstrumentSystem[F], gcalReader: GcalKeywordReader): Header =
    new Header {
      private val gcalKeywords = List(
        buildString(gcalReader.getLamp.orDefault, KeywordName.GCALLAMP),
        buildString(gcalReader.getFilter.orDefault, KeywordName.GCALFILT),
        buildString(gcalReader.getDiffuser.orDefault, KeywordName.GCALDIFF),
        buildString(gcalReader.getShutter.orDefault, KeywordName.GCALSHUT)
      )

      override def sendBefore(obsId: Observation.Id,
                              id: ImageFileId): SeqAction[Unit] =
        sendKeywords(id, inst, gcalKeywords)

      override def sendAfter(id: ImageFileId): SeqAction[Unit] = SeqAction(())
    }
}
