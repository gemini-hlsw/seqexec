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

case class GmosHeader(hs: DhsClient, gmosHeader: GmosHeader.InstKeywordsReader, tcsKeywordsReader: TcsKeywordsReader) extends Header {
  import Header._
  import Header.Defaults._
  override def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit] =  {
    sendKeywords(id, inst, hs, List(
      buildString(SeqAction(LocalDate.now.format(DateTimeFormatter.ISO_LOCAL_DATE)), "DATE-OBS"),
      buildString(tcsKeywordsReader.getUT, "TIME-OBS"),
      buildInt32(tcsKeywordsReader.getGmosInstPort, "INPORT"),
      buildString(gmosHeader.ccName, "GMOSCC"),
      // TODO PREIMAGE, NOD*
      buildInt32(gmosHeader.maskId, "MASKID"),
      buildString(gmosHeader.maskName, "MASKNAME"),
      buildInt32(gmosHeader.maskType, "MASKTYP")
    ))
  }

  override def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit] = SeqAction(())
}

object GmosHeader {
  trait InstKeywordsReader {
    def ccName: SeqAction[String]
    def maskId: SeqAction[Int]
    def maskName: SeqAction[String]
    def maskType: SeqAction[Int]
    def maskLoc: SeqAction[Int]
  }

  object DummyInstKeywordReader extends InstKeywordsReader {
    override def ccName: SeqAction[String] = SeqAction(Header.StrDefault)
    override def maskId: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def maskName: SeqAction[String] = SeqAction(Header.StrDefault)
    override def maskType: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def maskLoc: SeqAction[Int] = SeqAction(Header.IntDefault)
  }

  object InstKeywordReaderImpl extends InstKeywordsReader {
    implicit class String2SeqAction(val v: Option[String]) extends AnyVal {
      def toSeqAction: SeqAction[String] = SeqAction(v.getOrElse(Header.StrDefault))
    }

    implicit class Int2SeqAction(val v: Option[Int]) extends AnyVal {
      def toSeqAction: SeqAction[Int] = SeqAction(v.getOrElse(Header.IntDefault))
    }
    override def ccName: SeqAction[String] = GmosEpics.instance.ccName.toSeqAction
    override def maskId: SeqAction[Int] = GmosEpics.instance.maskId.toSeqAction
    override def maskName: SeqAction[String] = GmosEpics.instance.fpu.toSeqAction
    override def maskType: SeqAction[Int] = GmosEpics.instance.maskType.toSeqAction
    override def maskLoc: SeqAction[Int] = GmosEpics.instance.inBeam.toSeqAction
  }
}
