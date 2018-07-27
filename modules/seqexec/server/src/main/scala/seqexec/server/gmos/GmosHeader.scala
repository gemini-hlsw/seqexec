// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.ConfigUtilOps._
import seqexec.server.keywords._
import seqexec.server.InstrumentSystem
import seqexec.server.tcs.TcsKeywordsReader
import seqexec.server.{ConfigUtilOps, SeqAction, SeqexecFailure}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.IS_MOS_PREIMAGING_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import cats.data.EitherT
import cats.effect.IO
import cats.implicits._

object GmosHeader {
  // scalastyle:off
  def header(inst: InstrumentSystem[IO], gmosObsReader: GmosHeader.ObsKeywordsReader, gmosReader: GmosHeader.InstKeywordsReader, tcsKeywordsReader: TcsKeywordsReader): Header =
    new Header {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): SeqAction[Unit] = {
        sendKeywords(id, inst, List(
          buildInt32(tcsKeywordsReader.getGmosInstPort.orDefault, KeywordName.INPORT),
          buildString(gmosReader.ccName, KeywordName.GMOSCC),
          buildString(tcsKeywordsReader.getUT.orDefault, KeywordName.TIME_OBS),
          buildBoolean(gmosObsReader.preimage.map(_.toBoolean), KeywordName.PREIMAGE))
          // TODO NOD*
        )
      }

      private def adcKeywords =
        if (gmosReader.isADCInUse) {
          List(
            buildDouble(gmosReader.adcPrismEntSt, KeywordName.ADCENPST),
            buildDouble(gmosReader.adcPrismEntEnd, KeywordName.ADCENPEN),
            buildDouble(gmosReader.adcPrismEntMe, KeywordName.ADCENPME),
            buildDouble(gmosReader.adcPrismExtSt, KeywordName.ADCEXPST),
            buildDouble(gmosReader.adcPrismExtEnd, KeywordName.ADCEXPEN),
            buildDouble(gmosReader.adcPrismExtMe, KeywordName.ADCEXPME),
            buildDouble(gmosReader.adcWavelength1, KeywordName.ADCWLEN1),
            buildDouble(gmosReader.adcWavelength2, KeywordName.ADCWLEN2)
          )
        } else Nil

      private def roiKeywords = gmosReader.roiValues.map {
        case (i, rv) =>
          List(
            KeywordName.fromTag(s"DETRO${i}X").map(buildInt32(rv.xStart, _)),
            KeywordName.fromTag(s"DETRO${i}XS").map(buildInt32(rv.xSize, _)),
            KeywordName.fromTag(s"DETRO${i}Y").map(buildInt32(rv.yStart, _)),
            KeywordName.fromTag(s"DETRO${i}YS").map(buildInt32(rv.ySize, _))
          ).collect { case Some(x) => x }
      }.toList

      private val InBeam: Int = 0
      private def readMaskName: SeqAction[String] = gmosReader.maskLoc.flatMap{v => if(v === InBeam) gmosReader.maskName else SeqAction("None")}

      override def sendAfter(id: ImageFileId): SeqAction[Unit] = {
        sendKeywords(id, inst, List(
          buildInt32(gmosReader.maskId, KeywordName.MASKID),
          buildString(readMaskName, KeywordName.MASKNAME),
          buildInt32(gmosReader.maskType, KeywordName.MASKTYP),
          buildInt32(gmosReader.maskLoc, KeywordName.MASKLOC),
          buildString(gmosReader.filter1, KeywordName.FILTER1),
          buildInt32(gmosReader.filter1Id, KeywordName.FILTID1),
          buildString(gmosReader.filter2, KeywordName.FILTER2),
          buildInt32(gmosReader.filter2Id, KeywordName.FILTID2),
          buildString(gmosReader.grating, KeywordName.GRATING),
          buildInt32(gmosReader.gratingId, KeywordName.GRATID),
          buildDouble(gmosReader.gratingWavelength, KeywordName.GRWLEN),
          buildDouble(gmosReader.gratingAdjustedWavelength, KeywordName.CENTWAVE),
          buildInt32(gmosReader.gratingOrder, KeywordName.GRORDER),
          buildDouble(gmosReader.gratingTilt, KeywordName.GRTILT),
          buildDouble(gmosReader.gratingStep, KeywordName.GRSTEP),
          buildDouble(gmosReader.dtaX, KeywordName.DTAX),
          buildDouble(gmosReader.dtaY, KeywordName.DTAY),
          buildDouble(gmosReader.dtaZ, KeywordName.DTAZ),
          buildDouble(gmosReader.dtaZst, KeywordName.DTAZST),
          buildDouble(gmosReader.dtaZen, KeywordName.DTAZEN),
          buildDouble(gmosReader.dtaZme, KeywordName.DTAZME),
          buildString(gmosReader.stageMode, KeywordName.DTMODE),
          buildString(gmosReader.adcMode, KeywordName.ADCMODE),
          buildString(gmosReader.dcName, KeywordName.GMOSDC),
          buildString(gmosReader.detectorType, KeywordName.DETTYPE),
          buildString(gmosReader.detectorId, KeywordName.DETID),
          buildDouble(gmosReader.exposureTime, KeywordName.EXPOSURE),
          buildInt32(gmosReader.adcUsed, KeywordName.ADCUSED),
          buildInt32(gmosReader.detNRoi, KeywordName.DETNROI)
          // TODO These are enabled on N&S only
          /*buildInt32(gmosReader.aExpCount, "ANODCNT"),
          buildInt32(gmosReader.bExpCount, "BNODCNT"),
          buildInt32(gmosReader.exposureTime, "SUBINT")*/
        ) ::: adcKeywords ::: roiKeywords.flatten)
      }
    }
    // scalastyle:on

    final case class RoiValues(xStart: SeqAction[Int], xSize: SeqAction[Int], yStart: SeqAction[Int], ySize: SeqAction[Int])
    trait ObsKeywordsReader {
      def preimage: SeqAction[YesNoType]
    }

    final case class ObsKeywordsReaderImpl(config: Config) extends ObsKeywordsReader {
      override def preimage: SeqAction[YesNoType] = EitherT(IO.pure(config.extractAs[YesNoType](INSTRUMENT_KEY / IS_MOS_PREIMAGING_PROP)
        .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))))
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
      override def ccName: SeqAction[String] = SeqAction(StrDefault)
      override def maskId: SeqAction[Int] = SeqAction(IntDefault)
      override def maskName: SeqAction[String] = SeqAction(StrDefault)
      override def maskType: SeqAction[Int] = SeqAction(IntDefault)
      override def maskLoc: SeqAction[Int] = SeqAction(IntDefault)
      override def filter1: SeqAction[String] = SeqAction(StrDefault)
      override def filter2: SeqAction[String] = SeqAction(StrDefault)
      override def filter1Id: SeqAction[Int] = SeqAction(IntDefault)
      override def filter2Id: SeqAction[Int] = SeqAction(IntDefault)
      override def grating: SeqAction[String] = SeqAction(StrDefault)
      override def gratingId: SeqAction[Int] = SeqAction(IntDefault)
      override def gratingWavelength: SeqAction[Double] = SeqAction(DoubleDefault)
      override def gratingAdjustedWavelength: SeqAction[Double] = SeqAction(DoubleDefault)
      override def gratingOrder: SeqAction[Int] = SeqAction(IntDefault)
      override def gratingTilt: SeqAction[Double] = SeqAction(DoubleDefault)
      override def gratingStep: SeqAction[Double] = SeqAction(DoubleDefault)
      override def dtaX: SeqAction[Double] = SeqAction(DoubleDefault)
      override def dtaY: SeqAction[Double] = SeqAction(DoubleDefault)
      override def dtaZ: SeqAction[Double] = SeqAction(DoubleDefault)
      override def dtaZst: SeqAction[Double] = SeqAction(DoubleDefault)
      override def dtaZen: SeqAction[Double] = SeqAction(DoubleDefault)
      override def dtaZme: SeqAction[Double] = SeqAction(DoubleDefault)
      override def stageMode: SeqAction[String] = SeqAction(StrDefault)
      override def adcMode: SeqAction[String] = SeqAction(StrDefault)
      override def dcName: SeqAction[String] = SeqAction(StrDefault)
      override def detectorType: SeqAction[String] = SeqAction(StrDefault)
      override def detectorId: SeqAction[String] = SeqAction(StrDefault)
      override def exposureTime: SeqAction[Double] = SeqAction(DoubleDefault)
      override def adcUsed: SeqAction[Int] = SeqAction(IntDefault)
      override def adcPrismEntSt: SeqAction[Double] = SeqAction(DoubleDefault)
      override def adcPrismEntEnd: SeqAction[Double] = SeqAction(DoubleDefault)
      override def adcPrismEntMe: SeqAction[Double] = SeqAction(DoubleDefault)
      override def adcPrismExtSt: SeqAction[Double] = SeqAction(DoubleDefault)
      override def adcPrismExtEnd: SeqAction[Double] = SeqAction(DoubleDefault)
      override def adcPrismExtMe: SeqAction[Double] = SeqAction(DoubleDefault)
      override def adcWavelength1: SeqAction[Double] = SeqAction(DoubleDefault)
      override def adcWavelength2: SeqAction[Double] = SeqAction(DoubleDefault)
      override def detNRoi: SeqAction[Int] = SeqAction(IntDefault)
      override def roiValues: Map[Int, RoiValues] = Map.empty
      override def aExpCount: SeqAction[Int] = SeqAction(IntDefault)
      override def bExpCount: SeqAction[Int] = SeqAction(IntDefault)
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
