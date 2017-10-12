// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gcal

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.Header._
import edu.gemini.seqexec.server.Header.Implicits._
import edu.gemini.seqexec.server.{DhsClient, Header, SeqAction}

class GcalHeader(hs: DhsClient, gcalReader: GcalKeywordReader) extends Header {
  private val gcalKeywords = List(
    buildString(gcalReader.getLamp.orDefault, "GCALLAMP"),
    buildString(gcalReader.getFilter.orDefault, "GCALFILT"),
    buildString(gcalReader.getDiffuser.orDefault, "GCALDIFF"),
    buildString(gcalReader.getShutter.orDefault, "GCALSHUT")
  )

  override def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit] =
    sendKeywords(id, inst, hs, gcalKeywords)

  override def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit] = SeqAction(())
}
