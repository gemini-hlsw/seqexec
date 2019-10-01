// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.data.EitherT
import cats.{Applicative, MonadError}
import cats.effect.Sync
import cats.implicits._
import seqexec.server.ConfigUtilOps._
import seqexec.server.keywords._
import seqexec.server.gmos.GmosEpics.RoiStatus
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.IS_MOS_PREIMAGING_PROP
import seqexec.model.enum.NodAndShuffleStage.{StageA, StageB}
import seqexec.server.{CleanConfig, ConfigUtilOps, SeqexecFailure}
import seqexec.server.CleanConfig.extractItem
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.USE_NS_PROP
import gsp.math.{Angle, Offset}
import monocle.Getter
import seqexec.model.enum.NodAndShuffleStage

final case class RoiValues(xStart: Int, xSize: Int, yStart: Int, ySize: Int)

final case class GmosObsKeywordsReader[F[_]: MonadError[?[_], Throwable]](config: CleanConfig) {
  import GmosObsKeywordsReader._

  private implicit val BooleanDefaultValue: DefaultHeaderValue[Boolean] = DefaultHeaderValue.FalseDefaultValue

  def preimage: F[Boolean] = MonadError[F, Throwable].catchNonFatal(
    config
      .extractInstAs[YesNoType](IS_MOS_PREIMAGING_PROP)
      .getOrElse(YesNoType.NO)
      .toBoolean
  ).safeValOrDefault

  def nodMode: F[String] = "STANDARD".pure[F]

  def nodPix: F[Int] = Gmos.nodAndShuffle(config).map(_.rows: Int).explainExtractError[F]

  def nodCount: F[Int] = Gmos.nodAndShuffle(config).map(_.cycles: Int).explainExtractError[F]

  private def extractOffset(stage: NodAndShuffleStage, l: Getter[Offset, Angle]): F[Double] =
    Gmos.nodAndShuffle(config).explainExtractError[F]
      .flatMap(
        _.positions.find(_.stage === stage)
          .map{x => Angle.signedArcseconds.get(l.get(x.offset)).toDouble.pure[F]}
          .getOrElse(SeqexecFailure.Unexpected(s"Cannot find stage ${stage.symbol} parameters in step configuration.")
            .raiseError[F, Double]
          )
      )

  def nodAxOff: F[Double] = extractOffset(StageA, Offset.p.asGetter ^<-> Offset.P.angle)

  def nodAyOff: F[Double] = extractOffset(StageA, Offset.q.asGetter ^<-> Offset.Q.angle)

  def nodBxOff: F[Double] = extractOffset(StageB, Offset.p.asGetter ^<-> Offset.P.angle)

  def nodByOff: F[Double] = extractOffset(StageB, Offset.q.asGetter ^<-> Offset.Q.angle)

  def isNS: F[Boolean] = config.extractInstAs[java.lang.Boolean](USE_NS_PROP).map(_.booleanValue).explainExtractError[F]

}

object GmosObsKeywordsReader {

  private implicit class ExplainExtractError[A](v: Either[ExtractFailure, A]) {
    def explainExtractError[F[_]: MonadError[?[_], Throwable]]: F[A] =
      EitherT(v.pure[F])
        .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
        .widenRethrowT[Throwable]
  }

}

trait GmosKeywordReader[F[_]] {
  def ccName:                    F[String]
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
    override def roiValues: F[List[(Int, RoiValues)]] = listDefault[F, (Int, RoiValues)]
    override def aExpCount: F[Int]                    = intDefault[F]
    override def bExpCount: F[Int]                    = intDefault[F]
    override def isADCInUse: F[Boolean]               = boolDefault[F]
  }
}

object GmosKeywordReaderEpics {
  def apply[F[_]: Sync](sys: GmosEpics[F]): GmosKeywordReader[F] = new GmosKeywordReader[F] {

    override def ccName: F[String] = sys.ccName
    override def maskId: F[Int] = sys.maskId
    override def maskName: F[String] = sys.fpu
    override def maskType: F[Int] = sys.maskType
    override def maskLoc: F[Int] = sys.inBeam
    override def filter1: F[String] = sys.filter1
    override def filter2: F[String] = sys.filter2
    override def filter1Id: F[Int] = sys.filter1Id
    override def filter2Id: F[Int] = sys.filter2Id
    override def grating: F[String] = sys.disperser
    override def gratingId: F[Int] = sys.disperserId
    override def gratingWavelength: F[Double] = sys.gratingWavel
    override def gratingAdjustedWavelength: F[Double] = sys.disperserWavel
    override def gratingOrder: F[Int] = sys.disperserOrder
    override def gratingTilt: F[Double] = sys.gratingTilt
    override def gratingStep: F[Double] =
      // Set the value to the epics channel if inBeam is    1
      sys.disperserInBeam.map(_ === 1).ifM(sys.reqGratingMotorSteps, doubleDefault[F])
    override def dtaX: F[Double] = sys.dtaX
    override def dtaY: F[Double] = sys.dtaY
    override def dtaZ: F[Double] = sys.dtaZ
    override def dtaZst: F[Double] = sys.dtaZStart
    override def dtaZen: F[Double] = sys.dtaZEnd
    override def dtaZme: F[Double] = sys.dtaZMean
    override def stageMode: F[String] = sys.stageMode
    override def adcMode: F[String] = sys.adcMode
    override def dcName: F[String] = sys.dcName
    override def detectorType: F[String] = sys.detectorType
    override def detectorId: F[String] = sys.detectorId
    override def exposureTime: F[Double]   = sys.reqExposureTime.map(_.toDouble)
    override def adcUsed: F[Int]           = sys.adcUsed
    override def adcPrismEntSt: F[Double]  = sys.adcPrismEntryAngleStart
    override def adcPrismEntEnd: F[Double] = sys.adcPrismEntryAngleEnd
    override def adcPrismEntMe: F[Double]  = sys.adcPrismEntryAngleMean
    override def adcPrismExtSt: F[Double]  = sys.adcPrismExitAngleStart
    override def adcPrismExtEnd: F[Double] = sys.adcPrismEntryAngleEnd
    override def adcPrismExtMe: F[Double]  = sys.adcPrismExitAngleEnd
    override def adcWavelength1: F[Double] = sys.adcExitLowerWavel
    override def adcWavelength2: F[Double] = sys.adcExitUpperWavel
    // The TCL code does some verifications to ensure the value is not negative
    override def detNRoi: F[Int] =
      sys.roiNumUsed.map(_ > 0).ifM(sys.roiNumUsed, intDefault[F])

    private def roi(r: RoiStatus[F]): F[RoiValues] =
      (r.ccdXstart, r.ccdXsize, r.ccdYstart, r.ccdYsize).mapN(RoiValues.apply)

    private def readRois(count: Int, rois: Map[Int, RoiStatus[F]]): F[List[(Int, RoiValues)]] =
      (for {
        i <- (1 to count).toList
      } yield rois.get(i).traverse(roi).map(i -> _))
        .sequence.map {
          _.collect {
            case (i, Some(v)) => (i, v)
          }
        }

    override def roiValues: F[List[(Int, RoiValues)]] =
      (sys.roiNumUsed, sys.rois)
        .mapN(readRois)
        .flatten
        .handleError(_ => List.empty[(Int, RoiValues)])

    override def aExpCount: F[Int] = sys.aExpCount
    override def bExpCount: F[Int] = sys.aExpCount
    override def isADCInUse: F[Boolean] =
      sys.adcUsed.map(_ === 1)
        .handleError(_ => false)

  }
}
