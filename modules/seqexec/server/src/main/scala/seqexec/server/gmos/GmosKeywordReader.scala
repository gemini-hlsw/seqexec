// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.Applicative
import cats.effect.LiftIO
import cats.effect.Sync
import cats.implicits._
import seqexec.server.ConfigUtilOps._
import seqexec.server.keywords._
import seqexec.server.gmos.GmosEpics.RoiStatus
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.IS_MOS_PREIMAGING_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

final case class RoiValues(xStart: Int, xSize: Int, yStart: Int, ySize: Int)

final case class GmosObsKeywordsReader[F[_]: Sync](config: Config) {
  private implicit val BooleanDefaultValue = DefaultHeaderValue.FalseDefaultValue

  def preimage: F[Boolean] =
    Sync[F].delay(
        config
          .extractAs[YesNoType](INSTRUMENT_KEY / IS_MOS_PREIMAGING_PROP)
          .getOrElse(YesNoType.NO))
      .map(_.toBoolean)
      .safeValOrDefault
}

trait GmosKeywordReader[F[_]] {
  def ccName:                    F[String]
  // TODO Add NOD*
  /*def nodMode:                 F[String]
  def nodPix:                    F[Int]
  def nodCount:                  F[Int]
  def nodAxOff:                  F[Int]
  def nodAyOff:                  F[Int]
  def nodBxOff:                  F[Int]
  def nodByOff:                  F[Int]*/
  def maskId:                    F[Int]
  def maskName:                  F[String]
  def maskType:                  F[Int]
  def maskLoc:                   F[Int]
  def filter1:                   F[String]
  def filter2:                   F[String]
  def filter1Id:                 F[Int]
  def filter2Id:                 F[Int]
  def grating:                   F[String]
  def gratingId:                 F[Int]
  def gratingWavelength:         F[Double]
  def gratingAdjustedWavelength: F[Double]
  def gratingOrder:              F[Int]
  def gratingTilt:               F[Double]
  def gratingStep:               F[Double]
  def dtaX:                      F[Double]
  def dtaY:                      F[Double]
  def dtaZ:                      F[Double]
  def dtaZst:                    F[Double]
  def dtaZen:                    F[Double]
  def dtaZme:                    F[Double]
  def stageMode:                 F[String]
  def adcMode:                   F[String]
  def dcName:                    F[String]
  def detectorType:              F[String]
  def detectorId:                F[String]
  def exposureTime:              F[Double]
  def adcUsed:                   F[Int]
  def adcPrismEntSt:             F[Double]
  def adcPrismEntEnd:            F[Double]
  def adcPrismEntMe:             F[Double]
  def adcPrismExtSt:             F[Double]
  def adcPrismExtEnd:            F[Double]
  def adcPrismExtMe:             F[Double]
  def adcWavelength1:            F[Double]
  def adcWavelength2:            F[Double]
  def detNRoi:                   F[Int]
  def roiValues:                 F[List[(Int, RoiValues)]]
  def aExpCount:                 F[Int]
  def bExpCount:                 F[Int]
  def isADCInUse:                F[Boolean]
}

object GmosKeywordReaderDummy {
  def apply[F[_]: Applicative]: GmosKeywordReader[F] = new GmosKeywordReader[F] {
    override def ccName: F[String]                    = strDefault[F]
    override def maskId: F[Int]                       = intDefault[F]
    override def maskName: F[String]                  = strDefault[F]
    override def maskType: F[Int]                     = intDefault[F]
    override def maskLoc: F[Int]                      = intDefault[F]
    override def filter1: F[String]                   = strDefault[F]
    override def filter2: F[String]                   = strDefault[F]
    override def filter1Id: F[Int]                    = intDefault[F]
    override def filter2Id: F[Int]                    = intDefault[F]
    override def grating: F[String]                   = strDefault[F]
    override def gratingId: F[Int]                    = intDefault[F]
    override def gratingWavelength: F[Double]         = doubleDefault[F]
    override def gratingAdjustedWavelength: F[Double] = doubleDefault[F]
    override def gratingOrder: F[Int]                 = intDefault[F]
    override def gratingTilt: F[Double]               = doubleDefault[F]
    override def gratingStep: F[Double]               = doubleDefault[F]
    override def dtaX: F[Double]                      = doubleDefault[F]
    override def dtaY: F[Double]                      = doubleDefault[F]
    override def dtaZ: F[Double]                      = doubleDefault[F]
    override def dtaZst: F[Double]                    = doubleDefault[F]
    override def dtaZen: F[Double]                    = doubleDefault[F]
    override def dtaZme: F[Double]                    = doubleDefault[F]
    override def stageMode: F[String]                 = strDefault[F]
    override def adcMode: F[String]                   = strDefault[F]
    override def dcName: F[String]                    = strDefault[F]
    override def detectorType: F[String]              = strDefault[F]
    override def detectorId: F[String]                = strDefault[F]
    override def exposureTime: F[Double]              = doubleDefault[F]
    override def adcUsed: F[Int]                      = intDefault[F]
    override def adcPrismEntSt: F[Double]             = doubleDefault[F]
    override def adcPrismEntEnd: F[Double]            = doubleDefault[F]
    override def adcPrismEntMe: F[Double]             = doubleDefault[F]
    override def adcPrismExtSt: F[Double]             = doubleDefault[F]
    override def adcPrismExtEnd: F[Double]            = doubleDefault[F]
    override def adcPrismExtMe: F[Double]             = doubleDefault[F]
    override def adcWavelength1: F[Double]            = doubleDefault[F]
    override def adcWavelength2: F[Double]            = doubleDefault[F]
    override def detNRoi: F[Int]                      = intDefault[F]
    override def roiValues: F[List[(Int, RoiValues)]] =
      listDefault[F, (Int, RoiValues)]
    override def aExpCount: F[Int]                    = intDefault[F]
    override def bExpCount: F[Int]                    = intDefault[F]
    override def isADCInUse: F[Boolean]               = boolDefault[F]
  }
}

object GmosKeywordReaderEpics {
  // scalastyle:off
  def apply[F[_]: Sync: LiftIO]: GmosKeywordReader[F] = new GmosKeywordReader[F] {
    private val F = implicitly[Sync[F]]
    private val sys = GmosEpics.instance

    override def ccName: F[String] = F.delay(sys.ccName).safeValOrDefault
    override def maskId: F[Int] = F.delay(sys.maskId).safeValOrDefault
    override def maskName: F[String] = F.delay(sys.fpu).safeValOrDefault
    override def maskType: F[Int] = F.delay(sys.maskType).safeValOrDefault
    override def maskLoc: F[Int] = F.delay(sys.inBeam).safeValOrDefault
    override def filter1: F[String] = F.delay(sys.filter1).safeValOrDefault
    override def filter2: F[String] = F.delay(sys.filter2).safeValOrDefault
    override def filter1Id: F[Int] = F.delay(sys.filter1Id).safeValOrDefault
    override def filter2Id: F[Int] = F.delay(sys.filter2Id).safeValOrDefault
    override def grating: F[String] = F.delay(sys.disperser).safeValOrDefault
    override def gratingId: F[Int] = F.delay(sys.disperserId).safeValOrDefault
    override def gratingWavelength: F[Double] = F.delay(sys.gratingWavel).safeValOrDefault
    override def gratingAdjustedWavelength: F[Double] =F.delay(sys.disperserWavel).safeValOrDefault
    override def gratingOrder: F[Int] = F.delay(sys.disperserOrder).safeValOrDefault
    override def gratingTilt: F[Double] = F.delay(sys.gratingTilt).safeValOrDefault
    override def gratingStep: F[Double] =
      // Set the value to the epics channel if inBeam is    1
      F.delay(sys.reqGratingMotorSteps.filter(_ => sys.disperserInBeam === Some(1))).safeValOrDefault
    override def dtaX: F[Double] = F.delay(sys.dtaX).safeValOrDefault
    override def dtaY: F[Double] = F.delay(sys.dtaY).safeValOrDefault
    override def dtaZ: F[Double] = F.delay(sys.dtaZ).safeValOrDefault
    override def dtaZst: F[Double] = F.delay(sys.dtaZStart).safeValOrDefault
    override def dtaZen: F[Double] = F.delay(sys.dtaZEnd).safeValOrDefault
    override def dtaZme: F[Double] = F.delay(sys.dtaZMean).safeValOrDefault
    override def stageMode: F[String] = F.delay(sys.stageMode).safeValOrDefault
    override def adcMode: F[String] = F.delay(sys.adcMode).safeValOrDefault
    override def dcName: F[String] = F.delay(sys.dcName).safeValOrDefault
    override def detectorType: F[String] = F.delay(sys.detectorType).safeValOrDefault
    override def detectorId: F[String] = F.delay(sys.detectorId).safeValOrDefault
    // TODO Exposure changes with N&S
    override def exposureTime: F[Double]   = F.delay(sys.reqExposureTime.map(_.toDouble)).safeValOrDefault
    override def adcUsed: F[Int]           = F.delay(sys.adcUsed).safeValOrDefault
    override def adcPrismEntSt: F[Double]  = F.delay(sys.adcPrismEntryAngleStart).safeValOrDefault
    override def adcPrismEntEnd: F[Double] = F.delay(sys.adcPrismEntryAngleEnd).safeValOrDefault
    override def adcPrismEntMe: F[Double]  = F.delay(sys.adcPrismEntryAngleMean).safeValOrDefault
    override def adcPrismExtSt: F[Double]  = F.delay(sys.adcPrismExitAngleStart).safeValOrDefault
    override def adcPrismExtEnd: F[Double] = F.delay(sys.adcPrismEntryAngleEnd).safeValOrDefault
    override def adcPrismExtMe: F[Double]  = F.delay(sys.adcPrismExitAngleEnd).safeValOrDefault
    override def adcWavelength1: F[Double] = F.delay(sys.adcExitLowerWavel).safeValOrDefault
    override def adcWavelength2: F[Double] = F.delay(sys.adcExitUpperWavel).safeValOrDefault
    // The TCL code does some verifications to ensure the value is not negative
    override def detNRoi: F[Int] = F.delay(sys.roiNumUsed.filter(_ > 0).orEmpty).safeValOrDefault

    private def roi(r: RoiStatus): F[RoiValues] =
      (F.delay(r.ccdXstart).safeValOrDefault,
        F.delay(r.ccdXsize).safeValOrDefault,
        F.delay(r.ccdYstart).safeValOrDefault,
        F.delay(r.ccdYsize).safeValOrDefault).mapN(RoiValues.apply)

    private def readRois(count: Int, rois: Map[Int, RoiStatus]): F[List[(Int, RoiValues)]] =
      (for {
        i <- (1 to count).toList
      } yield rois.get(i).traverse(roi).map(i -> _))
        .sequence.map {
          _.collect {
            case (i, Some(v)) => (i, v)
          }
        }

    override def roiValues: F[List[(Int, RoiValues)]] =
      (F.delay(sys.roiNumUsed.orEmpty), F.delay(sys.rois))
        .mapN(readRois(_, _))
        .flatten
        .handleError(_ => Nil)

    override def aExpCount: F[Int] = F.delay(sys.aExpCount).safeValOrDefault
    override def bExpCount: F[Int] = F.delay(sys.aExpCount).safeValOrDefault
    override def isADCInUse: F[Boolean] =
      F.delay(sys.adcUsed.forall(_ === 1))
        .handleError(_ => false)
  }
  // scalastyle:on
}
