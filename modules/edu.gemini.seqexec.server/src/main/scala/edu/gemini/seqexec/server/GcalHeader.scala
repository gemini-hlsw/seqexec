// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.seqexec.model.dhs.ImageFileId
import Header._
import Header.Implicits._

class GcalHeader(hs: DhsClient, gcalReader: GcalKeywordReader) extends Header {
  val gcalKeywords = List(
    buildString(gcalReader.getLamp.orDefault, "GCALLAMP"),
    buildString(gcalReader.getFilter.orDefault, "GCALFILT"),
    buildString(gcalReader.getDiffuser.orDefault, "GCALDIFF"),
    buildString(gcalReader.getShutter.orDefault, "GCALSHUT")
  )

  override def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit] =
    sendKeywords(id, inst, hs, gcalKeywords)

  override def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit] = SeqAction(())
}

