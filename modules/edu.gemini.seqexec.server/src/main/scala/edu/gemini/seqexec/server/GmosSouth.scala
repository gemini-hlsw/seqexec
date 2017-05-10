package edu.gemini.seqexec.server

import java.util.logging.{Level, Logger}

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.spModel.config2.Config
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.spModel.gemini.gmos.InstGmosSouth.INSTRUMENT_NAME_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import edu.gemini.spModel.seqcomp.SeqConfigNames.OBSERVE_KEY
import edu.gemini.spModel.gemini.gmos.GmosCommonType._
import edu.gemini.spModel.gemini.gmos.GmosSouthType._
import edu.gemini.spModel.gemini.gmos.GmosSouthType.FPUnitSouth._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon._
import edu.gemini.spModel.gemini.gmos.InstGmosSouth._
import edu.gemini.spModel.core.Angle

import squants.space.LengthConversions._
import scalaz.{EitherT, Reader}
import scalaz.concurrent.Task

import scala.concurrent.duration._

final case class GmosSouth(controller: GmosSouthController) extends Instrument {
  import GmosSouth._

  override val name: String = INSTRUMENT_NAME_PROP

  override val sfName: String = "gmos"

  override val contributorName = "gmos"
  val dhsInstrumentName = "GMOS-S"

  val Log = Logger.getLogger(getClass.getName)

  override def observe(config: Config): SeqObserve[ImageFileId, ObserveResult] = Reader {
    fileId => controller.observe(fileId).map(_ => ObserveResult(fileId))
  }

  override def configure(config: Config): SeqAction[ConfigResult] =
    fromSequenceConfig(config).flatMap(controller.applyConfig).map(_ => ConfigResult(this))
}

object GmosSouth {
  import GmosSouthController._

  def fpuFromFPUnit(n: Option[FPU], m: Option[String])(fpu: FPUnitMode): GmosFPU = fpu match {
    case FPUnitMode.BUILTIN     => BuiltInFPU(n.getOrElse(FPU_NONE))
    case FPUnitMode.CUSTOM_MASK => m match {
      case Some(u) => CustomMaskFPU(u)
      case _       => UnknownFPU
    }
  }

  def ccConfigFromSequenceConfig(config: Config): TrySeq[CCConfig] =
    (for {
      posAngle         <- config.extract(INSTRUMENT_KEY / POS_ANGLE_PROP).as[java.lang.Double].map(d => Angle.fromDegrees(d.toDouble))
      filter           <- config.extract(INSTRUMENT_KEY / FILTER_PROP).as[Filter]
      disp             <- config.extract(INSTRUMENT_KEY / DISPERSER_PROP).as[Disperser]
      disperserOrder   =  config.extract(INSTRUMENT_KEY / DISPERSER_ORDER_PROP).as[DisperserOrder]
      disperserLambda  =  config.extract(INSTRUMENT_KEY / DISPERSER_LAMBDA_PROP).as[java.lang.Double].map(_.toDouble.nanometers)
      fpuName          =  config.extract(INSTRUMENT_KEY / FPU_PROP_NAME).as[FPU]
      fpuMask          =  config.extract(INSTRUMENT_KEY / FPU_MASK_PROP).as[String]
      fpu              <- config.extract(INSTRUMENT_KEY / FPU_MODE_PROP).as[FPUnitMode].map(fpuFromFPUnit(fpuName.toOption, fpuMask.toOption))
      stageMode        <- config.extract(INSTRUMENT_KEY / STAGE_MODE_PROP).as[GmosStageMode]
      dtax             <- config.extract(INSTRUMENT_KEY / DTAX_OFFSET_PROP).as[DTAX]
      adc              <- config.extract(INSTRUMENT_KEY / ADC_PROP).as[ADC]
      electronicOffset =  config.extract(INSTRUMENT_KEY / USE_ELECTRONIC_OFFSETTING_PROP).as[UseElectronicOffset]
      disperser = GmosDisperser(disp, disperserOrder.toOption, disperserLambda.toOption)
    } yield CCConfig(posAngle, filter, disperser, fpu, stageMode, dtax, adc, electronicOffset.toOption)).leftMap(e =>SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  def dcConfigFromSequenceConfig(config: Config): TrySeq[DCConfig] =
    (for {
      exposureTime <- config.extract(OBSERVE_KEY / EXPOSURE_TIME_PROP).as[java.lang.Double].map(_.toDouble.seconds)
      ampReadMode  <- config.extract(INSTRUMENT_KEY / AMP_READ_MODE_PROP).as[AmpReadMode]
      ampGain      <- config.extract(INSTRUMENT_KEY / AMP_GAIN_CHOICE_PROP).as[AmpGain]
      ampCount     <- config.extract(INSTRUMENT_KEY / AMP_COUNT_PROP).as[AmpCount]
      gainSetting  <- config.extract(INSTRUMENT_KEY / AMP_GAIN_SETTING_PROP).as[java.lang.Double]
      xBinning     <- config.extract(INSTRUMENT_KEY / CCD_X_BIN_PROP).as[Binning]
      yBinning     <- config.extract(INSTRUMENT_KEY / CCD_Y_BIN_PROP).as[Binning]
      builtInROI   <- config.extract(INSTRUMENT_KEY / BUILTIN_ROI_PROP).as[ROI]
      // TODO Add the custom ROI
    } yield DCConfig(exposureTime, CCDReadout(ampReadMode, ampGain, ampCount, gainSetting), CCDBinning(xBinning, yBinning), RegionsOfInterest(builtInROI, Nil))).leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  def fromSequenceConfig(config: Config): SeqAction[GmosSouthConfig] = EitherT( Task ( for {
      cc <- ccConfigFromSequenceConfig(config)
      dc <- dcConfigFromSequenceConfig(config)
    } yield GmosSouthConfig(cc, dc)
  ) )
 }
