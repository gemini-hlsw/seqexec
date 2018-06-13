// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import seqexec.model.dhs.ImageFileId
import seqexec.server.ConfigUtilOps._
import seqexec.server.Header.Implicits._
import seqexec.server.Header._
import seqexec.server.tcs.TcsKeywordsReader
import seqexec.server.{ConfigUtilOps, DhsClient, Header, SeqAction, SeqexecFailure}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.IS_MOS_PREIMAGING_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import cats.data.EitherT
import cats.effect.IO
import cats.implicits._

final case class GmosHeader(hs: DhsClient, gmosObsReader: GmosHeader.ObsKeywordsReader, gmosReader: GmosHeader.InstKeywordsReader, tcsKeywordsReader: TcsKeywordsReader) extends Header {
  override def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit] ={
    sendKeywords(id, inst, hs, List(
      buildInt32(tcsKeywordsReader.getGmosInstPort.orDefault, "INPORT"),
      buildString(gmosReader.ccName, "GMOSCC"),
      buildString(tcsKeywordsReader.getUT.orDefault, "TIME-OBS"),
      buildBoolean(gmosObsReader.preimage.map(_.toBoolean), "PREIMAGE"))
      // TODO NOD*
    )
  }

  private def adcKeywords =
    if (gmosReader.isADCInUse) {
      List(
        buildDouble(gmosReader.adcPrismEntSt, "ADCENPST"),
        buildDouble(gmosReader.adcPrismEntEnd, "ADCENPEN"),
        buildDouble(gmosReader.adcPrismEntMe, "ADCENPME"),
        buildDouble(gmosReader.adcPrismExtSt, "ADCEXPST"),
        buildDouble(gmosReader.adcPrismExtEnd, "ADCEXPEN"),
        buildDouble(gmosReader.adcPrismExtMe, "ADCEXPME"),
        buildDouble(gmosReader.adcWavelength1, "ADCWLEN1"),
        buildDouble(gmosReader.adcWavelength2, "ADCWLEN2")
      )
    } else Nil

  private def roiKeywords = gmosReader.roiValues.map {
    case (i, rv) =>
      List(
        buildInt32(rv.xStart, s"DETRO${i}X"),
        buildInt32(rv.xSize, s"DETRO${i}XS"),
        buildInt32(rv.yStart, s"DETRO${i}Y"),
        buildInt32(rv.ySize, s"DETRO${i}YS")
      )
  }.toList

  private val InBeam: Int = 0
  private def readMaskName: SeqAction[String] = gmosReader.maskLoc.flatMap{v => if(v === InBeam) gmosReader.maskName else SeqAction("None")}

  override def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit] = {
    sendKeywords(id, inst, hs, List(
      buildInt32(gmosReader.maskId, "MASKID"),
      buildString(readMaskName, "MASKNAME"),
      buildInt32(gmosReader.maskType, "MASKTYP"),
      buildInt32(gmosReader.maskLoc, "MASKLOC"),
      buildString(gmosReader.filter1, "FILTER1"),
      buildInt32(gmosReader.filter1Id, "FILTID1"),
      buildString(gmosReader.filter2, "FILTER2"),
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
      buildDouble(gmosReader.exposureTime, "EXPOSURE"),
      buildInt32(gmosReader.adcUsed, "ADCUSED"),
      buildInt32(gmosReader.detNRoi, "DETNROI")
      // TODO These are enabled on N&S only
      /*buildInt32(gmosReader.aExpCount, "ANODCNT"),
      buildInt32(gmosReader.bExpCount, "BNODCNT"),
      buildInt32(gmosReader.exposureTime, "SUBINT")*/
    ) ::: adcKeywords ::: roiKeywords.flatten)
  }

}

object GmosHeader {
  final case class RoiValues(xStart: SeqAction[Int], xSize: SeqAction[Int], yStart: SeqAction[Int], ySize: SeqAction[Int])
  trait ObsKeywordsReader {
    def preimage: SeqAction[YesNoType]
  }

  final case class ObsKeywordsReaderImpl(config: Config) extends ObsKeywordsReader {
    override def preimage: SeqAction[YesNoType] = EitherT(IO.pure(config.extract(INSTRUMENT_KEY / IS_MOS_PREIMAGING_PROP)
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
    def exposureTime: SeqAction[Double]
    def adcUsed: SeqAction[Int]
    def adcPrismEntSt: SeqAction[Double]
    def adcPrismEntEnd: SeqAction[Double]
    def adcPrismEntMe: SeqAction[Double]
    def adcPrismExtSt: SeqAction[Double]
    def adcPrismExtEnd: SeqAction[Double]
    def adcPrismExtMe: SeqAction[Double]
    def adcWavelength1: SeqAction[Double]
    def adcWavelength2: SeqAction[Double]
    def detNRoi: SeqAction[Int]
    def roiValues: Map[Int, RoiValues]
    def aExpCount: SeqAction[Int]
    def bExpCount: SeqAction[Int]
    def isADCInUse: Boolean
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
    override def exposureTime: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcUsed: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def adcPrismEntSt: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcPrismEntEnd: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcPrismEntMe: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcPrismExtSt: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcPrismExtEnd: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcPrismExtMe: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcWavelength1: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def adcWavelength2: SeqAction[Double] = SeqAction(Header.DoubleDefault)
    override def detNRoi: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def roiValues: Map[Int, RoiValues] = Map.empty
    override def aExpCount: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def bExpCount: SeqAction[Int] = SeqAction(Header.IntDefault)
    override def isADCInUse: Boolean = false
  }

  object InstKeywordReaderImpl extends InstKeywordsReader {

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
    override def gratingWavelength: SeqAction[Double] = GmosEpics.instance.gratingWavel.toSeqAction
    override def gratingAdjustedWavelength: SeqAction[Double] = GmosEpics.instance.disperserWavel.toSeqAction
    override def gratingOrder: SeqAction[Int] = GmosEpics.instance.disperserOrder.toSeqAction
    override def gratingTilt: SeqAction[Double] = GmosEpics.instance.gratingTilt.toSeqAction
    override def gratingStep: SeqAction[Double] =
      // Set the value to the epics channel if inBeam is    1
      GmosEpics.instance.reqGratingMotorSteps.filter(_ => GmosEpics.instance.disperserInBeam === Some(1)).toSeqAction
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
    // TODO Exposure changes with N&S
    override def exposureTime: SeqAction[Double] = GmosEpics.instance.reqExposureTime.map(_.toDouble).toSeqAction
    override def adcUsed: SeqAction[Int] = GmosEpics.instance.adcUsed.toSeqAction
    override def adcPrismEntSt: SeqAction[Double] = GmosEpics.instance.adcPrismEntryAngleStart.toSeqAction
    override def adcPrismEntEnd: SeqAction[Double] = GmosEpics.instance.adcPrismEntryAngleEnd.toSeqAction
    override def adcPrismEntMe: SeqAction[Double] = GmosEpics.instance.adcPrismEntryAngleMean.toSeqAction
    override def adcPrismExtSt: SeqAction[Double] = GmosEpics.instance.adcPrismExitAngleStart.toSeqAction
    override def adcPrismExtEnd: SeqAction[Double] = GmosEpics.instance.adcPrismEntryAngleEnd.toSeqAction
    override def adcPrismExtMe: SeqAction[Double] = GmosEpics.instance.adcPrismExitAngleEnd.toSeqAction
    override def adcWavelength1: SeqAction[Double] = GmosEpics.instance.adcExitLowerWavel.toSeqAction
    override def adcWavelength2: SeqAction[Double] = GmosEpics.instance.adcExitUpperWavel.toSeqAction
    // The TCL code does some verifications to ensure the value is not negative
    override def detNRoi: SeqAction[Int] = SeqAction(GmosEpics.instance.roiNumUsed.filter(_ > 0).getOrElse(0))
    override def roiValues: Map[Int, RoiValues] =
      (for {
        i <- 1 to GmosEpics.instance.roiNumUsed.getOrElse(0)
        roi = GmosEpics.instance.rois.get(i)
      } yield roi.map { r =>
          i ->
            RoiValues(r.ccdXstart.toSeqAction, r.ccdXsize.toSeqAction, r.ccdYstart.toSeqAction, r.ccdYsize.toSeqAction)
        }).toList.collect { case Some(x) => x }.toMap
    override def aExpCount: SeqAction[Int] = GmosEpics.instance.aExpCount.toSeqAction
    override def bExpCount: SeqAction[Int] = GmosEpics.instance.aExpCount.toSeqAction
    override def isADCInUse: Boolean =
      GmosEpics.instance.adcUsed.forall(_ === 1)
  }
}
