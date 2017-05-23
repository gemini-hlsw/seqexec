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
      buildInt32(gmosHeader.maskType, "MASKTYP"),
      buildString(gmosHeader.filter1, "FILTER1"),
      buildString(gmosHeader.filter2, "FILTER2"),
      buildInt32(gmosHeader.filter1Id, "FILTID1"),
      buildInt32(gmosHeader.filter2Id, "FILTID2"),
      buildString(gmosHeader.grating, "GRATING"),
      buildInt32(gmosHeader.gratingId, "GRATID"),
      buildDouble(gmosHeader.gratingWavelength, "GRWLEN"),
      buildDouble(gmosHeader.gratingAdjustedWavelength, "CENTWAVE"),
      buildInt32(gmosHeader.gratingOrder, "GRORDER"),
      buildDouble(gmosHeader.gratingTilt, "GRATILT"),
      buildDouble(gmosHeader.gratingStep, "GRASTEP")
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
    def filter1: SeqAction[String]
    def filter2: SeqAction[String]
    def filter1Id: SeqAction[Int]
    def filter2Id: SeqAction[Int]
    def grating: SeqAction[String]
    def gratingId: SeqAction[Int]
    def gratingWavelength: SeqAction[Double]
    def gratingAdjustedWavelength: SeqAction[Double]
    def gratingOrder: SeqAction[Int]
    def gratingTilt: SeqAction[Double]
    def gratingInBeam: SeqAction[Int]
    def gratingStep: SeqAction[Double]
    /*def gratingTurretA: SeqAction[String]
    def gratingTurretB: SeqAction[String]
    def gratingTurretC: SeqAction[String]
    def gratingTurretD: SeqAction[String]*/
  }

  object DummyInstKeywordReader extends InstKeywordsReader {
    override def ccName: SeqAction[String] = SeqAction(Header.StrDefault)
    override def maskId: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def maskName: SeqAction[String] = SeqAction(Header.StrDefault)
    override def maskType: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def maskLoc: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def filter1: SeqAction[String] = SeqAction(Header.StrDefault)
    override def filter2: SeqAction[String] = SeqAction(Header.StrDefault)
    override def filter1Id: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def filter2Id: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def grating: SeqAction[String] = SeqAction(Header.StrDefault)
    override def gratingId: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def gratingWavelength: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def gratingAdjustedWavelength: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def gratingOrder: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def gratingTilt: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def gratingInBeam: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def gratingStep: SeqAction[Double] = SeqAction(Header.DoubleDefault)
  }

  object InstKeywordReaderImpl extends InstKeywordsReader {
    implicit class String2SeqAction(val v: Option[String]) extends AnyVal {
      def toSeqAction: SeqAction[String] = SeqAction(v.getOrElse(Header.StrDefault))
    }

    implicit class Int2SeqAction(val v: Option[Int]) extends AnyVal {
      def toSeqAction: SeqAction[Int] = SeqAction(v.getOrElse(Header.IntDefault))
    }

    implicit class Double2SeqAction(val v: Option[Double]) extends AnyVal {
      def toSeqAction: SeqAction[Double] = SeqAction(v.getOrElse(Header.DoubleDefault))
    }
    override def ccName: SeqAction[String] = GmosEpics.instance.ccName.toSeqAction
    override def maskId: SeqAction[Int] = GmosEpics.instance.maskId.toSeqAction
    override def maskName: SeqAction[String] = GmosEpics.instance.fpu.toSeqAction
    override def maskType: SeqAction[Int] = GmosEpics.instance.maskType.toSeqAction
    override def maskLoc: SeqAction[Int] = GmosEpics.instance.inBeam.toSeqAction
    override def filter1: SeqAction[String] = GmosEpics.instance.filter1.toSeqAction
    override def filter2: SeqAction[String] = GmosEpics.instance.filter2.toSeqAction
    override def filter1Id: SeqAction[Int] = GmosEpics.instance.filter1Id.toSeqAction
    override def filter2Id: SeqAction[Int] = GmosEpics.instance.filter2Id.toSeqAction
    override def grating: SeqAction[String] = GmosEpics.instance.disperser.toSeqAction
    override def gratingId: SeqAction[Int] = GmosEpics.instance.disperserId.toSeqAction
    override def gratingWavelength: SeqAction[Double] = GmosEpics.instance.disperserWavel.toSeqAction
    override def gratingAdjustedWavelength: SeqAction[Double] = GmosEpics.instance.gratingWavel.toSeqAction
    override def gratingOrder: SeqAction[Int] = GmosEpics.instance.disperserOrder.toSeqAction
    override def gratingTilt: SeqAction[Double] = GmosEpics.instance.gratingTilt.toSeqAction
    override def gratingInBeam: SeqAction[Int] = GmosEpics.instance.disperserInBeam.toSeqAction
    override def gratingStep: SeqAction[Double] = GmosEpics.instance.reqGratingMotorSteps.toSeqAction
    // TODO Implement gratingTurrent*
    /*override def gratingTurretA: SeqAction[String] =
    override def gratingTurretB: SeqAction[String] =
    override def gratingTurretC: SeqAction[String] =
    override def gratingTurretD: SeqAction[String] =*/
  }
}
