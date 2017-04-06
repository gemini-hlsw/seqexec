package edu.gemini.seqexec.server

import edu.gemini.seqexec.model.dhs.ImageFileId
import Header._
import Header.Defaults._
import KeywordsReader._

class GcalHeader(hs: DhsClient, gcalReader: GcalKeywordReader) extends Header {
  val gcalKeywords = List(
      buildString(gcalReader.getDiffuser, "GCALDIFF"),
      buildString(gcalReader.getFilter, "GCALFILT"),
      buildString(gcalReader.getLamp, "GCALLAMP"),
      buildString(gcalReader.getShutter, "GCALSHUT")
    )

  override def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit] =
    sendKeywords(id, inst, hs, gcalKeywords)

  override def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit] = SeqAction(())
}

