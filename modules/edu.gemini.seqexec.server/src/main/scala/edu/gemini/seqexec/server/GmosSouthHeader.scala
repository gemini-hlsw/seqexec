package edu.gemini.seqexec.server

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.{MOS_PREIMAGING_PROP, READMODE_PROP, ReadMode}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

import scalaz.EitherT
import scalaz.concurrent.Task

case class GmosSouthHeader(hs: DhsClient, tcsKeywordsReader: TcsKeywordsReader) extends Header {
  import Header._
  import Header.Defaults._
  override def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit] =  {
    sendKeywords(id, inst, hs, List(
      buildString(SeqAction(LocalDate.now.format(DateTimeFormatter.ISO_LOCAL_DATE)), "DATE-OBS"),
      buildString(tcsKeywordsReader.getUT, "TIME-OBS")
    ))
  }

  override def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit] = SeqAction(())
}

