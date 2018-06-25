// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import seqexec.model.dhs.ImageFileId
import seqexec.server.HeaderProvider
import seqexec.server.Header._
import seqexec.server.Header.Implicits._
import seqexec.server.{Header, SeqAction}
import seqexec.server.keywords.DhsClient

object GcalHeader {
  implicit def header(hs: DhsClient, gcalReader: GcalKeywordReader): Header =
    new Header {
      private val gcalKeywords = List(
        buildString(gcalReader.getLamp.orDefault, "GCALLAMP"),
        buildString(gcalReader.getFilter.orDefault, "GCALFILT"),
        buildString(gcalReader.getDiffuser.orDefault, "GCALDIFF"),
        buildString(gcalReader.getShutter.orDefault, "GCALSHUT")
      )

      override def sendBefore[A: HeaderProvider](id: ImageFileId, inst: A): SeqAction[Unit] =
        sendKeywords(id, inst, hs, gcalKeywords)

      override def sendAfter[A: HeaderProvider](id: ImageFileId, inst: A): SeqAction[Unit] = SeqAction(())
    }
}
