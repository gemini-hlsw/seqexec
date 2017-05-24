package edu.gemini.seqexec.server

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.IS_MOS_PREIMAGING_PROP
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

case class GmosHeader(hs: DhsClient, gmosObsReader: GmosHeader.ObsKeywordsReader, gmosReader: GmosHeader.InstKeywordsReader, tcsKeywordsReader: TcsKeywordsReader) extends Header {
  import Header._
  import Header.Defaults._
  override def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit] =  {
    def adcKeywords = {
      if (GmosEpics.instance.adcUsed.forall(_ == true)) {
        List(
          buildDouble(gmosReader.adcPrismEntSt, "ADCENPST"),
          buildDouble(gmosReader.adcPrismEntEnd, "ADCENPEN"),
          buildDouble(gmosReader.adcPrismEntSt, "ADCENPST"),
          buildDouble(gmosReader.adcPrismExtEnd, "ADCEXPEN"),
          buildDouble(gmosReader.adcPrismExtMe, "ADCEXPME"),
          buildDouble(gmosReader.adcPrismExtMe, "ADCEXPME"),
          buildDouble(gmosReader.adcWavelength1, "ADCWLEN1"),
          buildDouble(gmosReader.adcWavelength2, "ADCWLEN2")
        )
      } else Nil
    }
    sendKeywords(id, inst, hs, List(
      buildString(SeqAction(LocalDate.now.format(DateTimeFormatter.ISO_LOCAL_DATE)), "DATE-OBS"),
      buildString(tcsKeywordsReader.getUT, "TIME-OBS"),
      buildInt32(tcsKeywordsReader.getGmosInstPort, "INPORT"),
      buildString(gmosReader.ccName, "GMOSCC"),
      buildBoolean(gmosObsReader.preimage.map(_.toBoolean), "PREIMAGE"),
      // TODO NOD*
      buildInt32(gmosReader.maskId, "MASKID"),
      buildString(gmosReader.maskName, "MASKNAME"),
      buildInt32(gmosReader.maskType, "MASKTYP"),
      buildString(gmosReader.filter1, "FILTER1"),
      buildString(gmosReader.filter2, "FILTER2"),
      buildInt32(gmosReader.filter1Id, "FILTID1"),
      buildInt32(gmosReader.filter2Id, "FILTID2"),
      buildString(gmosReader.grating, "GRATING"),
      buildInt32(gmosReader.gratingId, "GRATID"),
      buildDouble(gmosReader.gratingWavelength, "GRWLEN"),
      buildDouble(gmosReader.gratingAdjustedWavelength, "CENTWAVE"),
      buildInt32(gmosReader.gratingOrder, "GRORDER"),
      buildDouble(gmosReader.gratingTilt, "GRTILT"),
      buildDouble(gmosReader.gratingStep, "GRSTEP"),
      buildDouble(gmosReader.dtaX, "DTAX"),
      buildDouble(gmosReader.dtaY, "DTAY"),
      buildDouble(gmosReader.dtaZ, "DTAZ"),
      buildDouble(gmosReader.dtaZst, "DTAZST"),
      buildDouble(gmosReader.dtaZen, "DTAZEN"),
      buildDouble(gmosReader.dtaZme, "DTAZME"),
      buildString(gmosReader.stageMode, "DTMODE"),
      buildString(gmosReader.adcMode, "ADCMODE"),
      buildString(gmosReader.dcName, "GMOSDC"),
      buildString(gmosReader.detectorType, "DETTYPE"),
      buildString(gmosReader.detectorId, "DETID"),
      buildInt32(gmosReader.exposureTime, "EXPOSURE"),
      buildInt32(gmosReader.adcUsed, "ADCUSED")
    ) ++ adcKeywords)
  }

  override def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit] = SeqAction(())
}

object GmosHeader {
  trait ObsKeywordsReader {
    def preimage: SeqAction[YesNoType]
  }

  case class ObsKeywordsReaderImpl(config: Config) extends ObsKeywordsReader {
    override def preimage: SeqAction[YesNoType] = EitherT(Task.now(config.extract(INSTRUMENT_KEY / IS_MOS_PREIMAGING_PROP)
      .as[YesNoType].leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))))
  }

  trait InstKeywordsReader {
    def ccName: SeqAction[String]
    // TODO Add NOD*
    /*def nodMode: SeqAction[String]
    def nodPix: SeqAction[Int]
    def nodCount: SeqAction[Int]
    def nodAxOff: SeqAction[Int]
    def nodAyOff: SeqAction[Int]
    def nodBxOff: SeqAction[Int]
    def nodByOff: SeqAction[Int]*/
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
    def gratingStep: SeqAction[Double]
    def dtaX: SeqAction[Double]
    def dtaY: SeqAction[Double]
    def dtaZ: SeqAction[Double]
    def dtaZst: SeqAction[Double]
    def dtaZen: SeqAction[Double]
    def dtaZme: SeqAction[Double]
    def stageMode: SeqAction[String]
    def adcMode: SeqAction[String]
    def dcName: SeqAction[String]
    def detectorType: SeqAction[String]
    def detectorId: SeqAction[String]
    def exposureTime: SeqAction[Int]
    def adcUsed: SeqAction[Int]
    def adcPrismEntSt: SeqAction[Double]
    def adcPrismEntEnd: SeqAction[Double]
    def adcPrismEntMe: SeqAction[Double]
    def adcPrismExtSt: SeqAction[Double]
    def adcPrismExtEnd: SeqAction[Double]
    def adcPrismExtMe: SeqAction[Double]
    def adcWavelength1: SeqAction[Double]
    def adcWavelength2: SeqAction[Double]
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
    override def gratingStep: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def dtaX: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def dtaY: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def dtaZ: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def dtaZst: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def dtaZen: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def dtaZme: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def stageMode: SeqAction[String] = SeqAction(Header.StrDefault)
    override def adcMode: SeqAction[String] = SeqAction(Header.StrDefault)
    override def dcName: SeqAction[String] = SeqAction(Header.StrDefault)
    override def detectorType: SeqAction[String] = SeqAction(Header.StrDefault)
    override def detectorId: SeqAction[String] = SeqAction(Header.StrDefault)
    override def exposureTime: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def adcUsed: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def adcPrismEntSt: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcPrismEntEnd: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcPrismEntMe: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcPrismExtSt: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcPrismExtEnd: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcPrismExtMe: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcWavelength1: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcWavelength2: SeqAction[Double] = SeqAction(Header.DoubleDefault)
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
    override def gratingStep: SeqAction[Double] =
      GmosEpics.instance.reqGratingMotorSteps.filter(_ => GmosEpics.instance.inBeam === Some(1)).toSeqAction
    override def dtaX: SeqAction[Double] = GmosEpics.instance.dtaX.toSeqAction
    override def dtaY: SeqAction[Double] = GmosEpics.instance.dtaY.toSeqAction
    override def dtaZ: SeqAction[Double] = GmosEpics.instance.dtaZ.toSeqAction
    override def dtaZst: SeqAction[Double] = GmosEpics.instance.dtaZStart.toSeqAction
    override def dtaZen: SeqAction[Double] = GmosEpics.instance.dtaZEnd.toSeqAction
    override def dtaZme: SeqAction[Double] = GmosEpics.instance.dtaZMean.toSeqAction
    override def stageMode: SeqAction[String] = GmosEpics.instance.stageMode.toSeqAction
    override def adcMode: SeqAction[String] = GmosEpics.instance.adcMode.toSeqAction
    override def dcName: SeqAction[String] = GmosEpics.instance.dcName.toSeqAction
    override def detectorType: SeqAction[String] = GmosEpics.instance.detectorType.toSeqAction
    override def detectorId: SeqAction[String] = GmosEpics.instance.detectorId.toSeqAction
    override def exposureTime: SeqAction[Int] = GmosEpics.instance.reqExposureTime.toSeqAction
    override def adcUsed: SeqAction[Int] = GmosEpics.instance.adcUsed.toSeqAction
    override def adcPrismEntSt: SeqAction[Double] = GmosEpics.instance.adcPrismEntryAngleStart.toSeqAction
    override def adcPrismEntEnd: SeqAction[Double] = GmosEpics.instance.adcPrismEntryAngleEnd.toSeqAction
    override def adcPrismEntMe: SeqAction[Double] = GmosEpics.instance.adcPrismEntryAngleMean.toSeqAction
    override def adcPrismExtSt: SeqAction[Double] = GmosEpics.instance.adcPrismExitAngleStart.toSeqAction
    override def adcPrismExtEnd: SeqAction[Double] = GmosEpics.instance.adcPrismEntryAngleEnd.toSeqAction
    override def adcPrismExtMe: SeqAction[Double] = GmosEpics.instance.adcPrismExitAngleEnd.toSeqAction
    override def adcWavelength1: SeqAction[Double] = GmosEpics.instance.adcExitLowerWavel.toSeqAction
    override def adcWavelength2: SeqAction[Double] = GmosEpics.instance.adcExitUpperWavel.toSeqAction
    // TODO Implement gratingTurrent*
    /*override def gratingTurretA: SeqAction[String] =
    override def gratingTurretB: SeqAction[String] =
    override def gratingTurretC: SeqAction[String] =
    override def gratingTurretD: SeqAction[String] =*/
  }
}
