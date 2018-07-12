// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.SeqAction

object GcalHeader {
  implicit def header[A: HeaderProvider](inst: A, gcalReader: GcalKeywordReader): Header =
    new Header {
      private val gcalKeywords = List(
        buildString(gcalReader.getLamp.orDefault, "GCALLAMP"),
        buildString(gcalReader.getFilter.orDefault, "GCALFILT"),
        buildString(gcalReader.getDiffuser.orDefault, "GCALDIFF"),
        buildString(gcalReader.getShutter.orDefault, "GCALSHUT")
      )

      override def sendBefore(id: ImageFileId): SeqAction[Unit] =
        sendKeywords(id, inst, gcalKeywords)

      override def sendAfter(id: ImageFileId): SeqAction[Unit] = SeqAction(())
    }
}
