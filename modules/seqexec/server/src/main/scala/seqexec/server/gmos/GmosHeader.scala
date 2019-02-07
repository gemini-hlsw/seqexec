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
import seqexec.server.{ConfigUtilOps, SeqAction, SeqActionF, SeqexecFailure}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.IS_MOS_PREIMAGING_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import cats.data.EitherT
import cats.effect.IO
import cats.effect.Sync
import cats.implicits._

object GmosHeader {
  // scalastyle:off
  def header[F[_]: Sync](inst: InstrumentSystem[F], gmosObsReader: GmosHeader.ObsKeywordsReader[F], gmosReader: GmosHeader.InstKeywordsReader[F], tcsKeywordsReader: TcsKeywordsReader[F]): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): SeqActionF[F, Unit] = {
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
      private def readMaskName: SeqActionF[F, String] = gmosReader.maskLoc.flatMap{v => if(v === InBeam) gmosReader.maskName else SeqActionF("None")}

      override def sendAfter(id: ImageFileId): SeqActionF[F, Unit] = {
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

    final case class RoiValues[F[_]](xStart: SeqActionF[F, Int], xSize: SeqActionF[F, Int], yStart: SeqActionF[F, Int], ySize: SeqActionF[F, Int])
    trait ObsKeywordsReader[F[_]] {
      def preimage: SeqActionF[F, YesNoType]
    }

    final case class ObsKeywordsReaderImpl[F[_]: Sync](config: Config) extends ObsKeywordsReader[F] {
      override def preimage: SeqActionF[F, YesNoType] = EitherT(Sync[F].delay(config.extractAs[YesNoType](INSTRUMENT_KEY / IS_MOS_PREIMAGING_PROP)
        .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))))
    }

    trait InstKeywordsReader[F[_]] {
      def ccName: SeqActionF[F, String]
      // TODO Add NOD*
      /*def nodMode: SeqActionF[F, String]
      def nodPix: SeqActionF[F, Int]
      def nodCount: SeqActionF[F, Int]
      def nodAxOff: SeqActionF[F, Int]
      def nodAyOff: SeqActionF[F, Int]
      def nodBxOff: SeqActionF[F, Int]
      def nodByOff: SeqActionF[F, Int]*/
      def maskId: SeqActionF[F, Int]
      def maskName: SeqActionF[F, String]
      def maskType: SeqActionF[F, Int]
      def maskLoc: SeqActionF[F, Int]
      def filter1: SeqActionF[F, String]
      def filter2: SeqActionF[F, String]
      def filter1Id: SeqActionF[F, Int]
      def filter2Id: SeqActionF[F, Int]
      def grating: SeqActionF[F, String]
      def gratingId: SeqActionF[F, Int]
      def gratingWavelength: SeqActionF[F, Double]
      def gratingAdjustedWavelength: SeqActionF[F, Double]
      def gratingOrder: SeqActionF[F, Int]
      def gratingTilt: SeqActionF[F, Double]
      def gratingStep: SeqActionF[F, Double]
      def dtaX: SeqActionF[F, Double]
      def dtaY: SeqActionF[F, Double]
      def dtaZ: SeqActionF[F, Double]
      def dtaZst: SeqActionF[F, Double]
      def dtaZen: SeqActionF[F, Double]
      def dtaZme: SeqActionF[F, Double]
      def stageMode: SeqActionF[F, String]
      def adcMode: SeqActionF[F, String]
      def dcName: SeqActionF[F, String]
      def detectorType: SeqActionF[F, String]
      def detectorId: SeqActionF[F, String]
      def exposureTime: SeqActionF[F, Double]
      def adcUsed: SeqActionF[F, Int]
      def adcPrismEntSt: SeqActionF[F, Double]
      def adcPrismEntEnd: SeqActionF[F, Double]
      def adcPrismEntMe: SeqActionF[F, Double]
      def adcPrismExtSt: SeqActionF[F, Double]
      def adcPrismExtEnd: SeqActionF[F, Double]
      def adcPrismExtMe: SeqActionF[F, Double]
      def adcWavelength1: SeqActionF[F, Double]
      def adcWavelength2: SeqActionF[F, Double]
      def detNRoi: SeqActionF[F, Int]
      def roiValues: Map[Int, RoiValues[F]]
      def aExpCount: SeqActionF[F, Int]
      def bExpCount: SeqActionF[F, Int]
      def isADCInUse: Boolean
    }

    object DummyInstKeywordReader extends InstKeywordsReader[IO] {
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
      override def roiValues: Map[Int, RoiValues[IO]] = Map.empty
      override def aExpCount: SeqAction[Int] = SeqAction(IntDefault)
      override def bExpCount: SeqAction[Int] = SeqAction(IntDefault)
      override def isADCInUse: Boolean = false
    }

    object InstKeywordReaderImpl extends InstKeywordsReader[IO] {

      override def ccName: SeqAction[String] = GmosEpics.instance.ccName.toSeqActionDefault
      override def maskId: SeqAction[Int] = GmosEpics.instance.maskId.toSeqActionDefault
      override def maskName: SeqAction[String] = GmosEpics.instance.fpu.toSeqActionDefault
      override def maskType: SeqAction[Int] = GmosEpics.instance.maskType.toSeqActionDefault
      override def maskLoc: SeqAction[Int] = GmosEpics.instance.inBeam.toSeqActionDefault
      override def filter1: SeqAction[String] = GmosEpics.instance.filter1.toSeqActionDefault
      override def filter2: SeqAction[String] = GmosEpics.instance.filter2.toSeqActionDefault
      override def filter1Id: SeqAction[Int] = GmosEpics.instance.filter1Id.toSeqActionDefault
      override def filter2Id: SeqAction[Int] = GmosEpics.instance.filter2Id.toSeqActionDefault
      override def grating: SeqAction[String] = GmosEpics.instance.disperser.toSeqActionDefault
      override def gratingId: SeqAction[Int] = GmosEpics.instance.disperserId.toSeqActionDefault
      override def gratingWavelength: SeqAction[Double] = GmosEpics.instance.gratingWavel.toSeqActionDefault
      override def gratingAdjustedWavelength: SeqAction[Double] = GmosEpics.instance.disperserWavel.toSeqActionDefault
      override def gratingOrder: SeqAction[Int] = GmosEpics.instance.disperserOrder.toSeqActionDefault
      override def gratingTilt: SeqAction[Double] = GmosEpics.instance.gratingTilt.toSeqActionDefault
      override def gratingStep: SeqAction[Double] =
        // Set the value to the epics channel if inBeam is    1
        GmosEpics.instance.reqGratingMotorSteps.filter(_ => GmosEpics.instance.disperserInBeam === Some(1)).toSeqActionDefault
      override def dtaX: SeqAction[Double] = GmosEpics.instance.dtaX.toSeqActionDefault
      override def dtaY: SeqAction[Double] = GmosEpics.instance.dtaY.toSeqActionDefault
      override def dtaZ: SeqAction[Double] = GmosEpics.instance.dtaZ.toSeqActionDefault
      override def dtaZst: SeqAction[Double] = GmosEpics.instance.dtaZStart.toSeqActionDefault
      override def dtaZen: SeqAction[Double] = GmosEpics.instance.dtaZEnd.toSeqActionDefault
      override def dtaZme: SeqAction[Double] = GmosEpics.instance.dtaZMean.toSeqActionDefault
      override def stageMode: SeqAction[String] = GmosEpics.instance.stageMode.toSeqActionDefault
      override def adcMode: SeqAction[String] = GmosEpics.instance.adcMode.toSeqActionDefault
      override def dcName: SeqAction[String] = GmosEpics.instance.dcName.toSeqActionDefault
      override def detectorType: SeqAction[String] = GmosEpics.instance.detectorType.toSeqActionDefault
      override def detectorId: SeqAction[String] = GmosEpics.instance.detectorId.toSeqActionDefault
      // TODO Exposure changes with N&S
      override def exposureTime: SeqAction[Double] = GmosEpics.instance.reqExposureTime.map(_.toDouble).toSeqActionDefault
      override def adcUsed: SeqAction[Int] = GmosEpics.instance.adcUsed.toSeqActionDefault
      override def adcPrismEntSt: SeqAction[Double] = GmosEpics.instance.adcPrismEntryAngleStart.toSeqActionDefault
      override def adcPrismEntEnd: SeqAction[Double] = GmosEpics.instance.adcPrismEntryAngleEnd.toSeqActionDefault
      override def adcPrismEntMe: SeqAction[Double] = GmosEpics.instance.adcPrismEntryAngleMean.toSeqActionDefault
      override def adcPrismExtSt: SeqAction[Double] = GmosEpics.instance.adcPrismExitAngleStart.toSeqActionDefault
      override def adcPrismExtEnd: SeqAction[Double] = GmosEpics.instance.adcPrismEntryAngleEnd.toSeqActionDefault
      override def adcPrismExtMe: SeqAction[Double] = GmosEpics.instance.adcPrismExitAngleEnd.toSeqActionDefault
      override def adcWavelength1: SeqAction[Double] = GmosEpics.instance.adcExitLowerWavel.toSeqActionDefault
      override def adcWavelength2: SeqAction[Double] = GmosEpics.instance.adcExitUpperWavel.toSeqActionDefault
      // The TCL code does some verifications to ensure the value is not negative
      override def detNRoi: SeqAction[Int] = SeqAction(GmosEpics.instance.roiNumUsed.filter(_ > 0).getOrElse(0))
      override def roiValues: Map[Int, RoiValues[IO]] =
        (for {
          i <- 1 to GmosEpics.instance.roiNumUsed.getOrElse(0)
          roi = GmosEpics.instance.rois.get(i)
        } yield roi.map { r =>
            i ->
              RoiValues(r.ccdXstart.toSeqActionDefault, r.ccdXsize.toSeqActionDefault, r.ccdYstart.toSeqActionDefault, r.ccdYsize.toSeqActionDefault)
          }).toList.collect { case Some(x) => x }.toMap
      override def aExpCount: SeqAction[Int] = GmosEpics.instance.aExpCount.toSeqActionDefault
      override def bExpCount: SeqAction[Int] = GmosEpics.instance.aExpCount.toSeqActionDefault
      override def isADCInUse: Boolean =
        GmosEpics.instance.adcUsed.forall(_ === 1)
    }
}
