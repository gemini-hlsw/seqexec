package edu.gemini.seqexec.server

import java.util.logging.Logger

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.ConfigUtilOps.{ContentError, ConversionError}
import edu.gemini.seqexec.server.GmosController.{ADC, DTAX, DisperserOrder, GmosFPU, UseElectronicOffset}
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.GmosCommonType._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon._
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames.{INSTRUMENT_KEY, OBSERVE_KEY}
import edu.gemini.spModel.obscomp.InstConstants.EXPOSURE_TIME_PROP
import squants.space.LengthConversions._

import scalaz.{EitherT, Reader, \/}
import scalaz.concurrent.Task
import scalaz.syntax.std.string._
import scala.concurrent.duration._

/**
  * Created by jluhrs on 8/3/17.
  */
abstract class Gmos[T<:GmosController](val controller: T) extends Instrument {
  import Gmos._

  override val sfName: String = "gmos"

  override val contributorName = "gmosdc"

  val Log = Logger.getLogger(getClass.getName)

  protected def fpuFromFPUnit(n: Option[controller.FPU], m: Option[String])(fpu: FPUnitMode): GmosFPU

  protected def extractFilter(config: Config): ExtractFailure\/controller.Filter

  protected def extractDisperser(config: Config): ExtractFailure\/controller.Disperser

  protected def extractFPU(config: Config): ExtractFailure\/controller.FPU

  protected def extractStageMode(config: Config): ExtractFailure\/controller.GmosStageMode

  private def ccConfigFromSequenceConfig(config: Config): TrySeq[controller.CCConfig] =
    (for {
      filter           <- extractFilter(config)
      disp             <- extractDisperser(config)
      disperserOrder   =  config.extract(INSTRUMENT_KEY / DISPERSER_ORDER_PROP).as[DisperserOrder]
      disperserLambda  =  config.extract(INSTRUMENT_KEY / DISPERSER_LAMBDA_PROP).as[java.lang.Double].map(_.toDouble.nanometers)
      fpuName          =  extractFPU(config)
      fpuMask          =  config.extract(INSTRUMENT_KEY / FPU_MASK_PROP).as[String]
      fpu              <- config.extract(INSTRUMENT_KEY / FPU_MODE_PROP).as[FPUnitMode].map(fpuFromFPUnit(fpuName.toOption, fpuMask.toOption))
      stageMode        <- extractStageMode(config)
      dtax             <- config.extract(INSTRUMENT_KEY / DTAX_OFFSET_PROP).as[DTAX]
      adc              <- config.extract(INSTRUMENT_KEY / ADC_PROP).as[ADC]
      electronicOffset =  config.extract(INSTRUMENT_KEY / USE_ELECTRONIC_OFFSETTING_PROP).as[UseElectronicOffset]
      disperser = controller.GmosDisperser(disp, disperserOrder.toOption, disperserLambda.toOption)
    } yield controller.CCConfig(filter, disperser, fpu, stageMode, dtax, adc, electronicOffset.toOption)).leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  private def fromSequenceConfig(config: Config): SeqAction[controller.GmosConfig] = EitherT( Task ( for {
      cc <- ccConfigFromSequenceConfig(config)
      dc <- dcConfigFromSequenceConfig(config)
    } yield controller.GmosConfig(cc, dc)
  ) )

  override def observe(config: Config): SeqObserve[ImageFileId, ObserveResult] = Reader {
    fileId => controller.observe(fileId).map(_ => ObserveResult(fileId))
  }

  override def configure(config: Config): SeqAction[ConfigResult] =
    fromSequenceConfig(config).flatMap(controller.applyConfig).map(_ => ConfigResult(this))
}

object Gmos {
  val name: String = INSTRUMENT_NAME_PROP

  import GmosController._

  // It seems this is unused but it shows up on the DC apply config
  private def biasTimeObserveType(observeType: String): BiasTime = observeType match {
    case SCIENCE_OBSERVE_TYPE => BiasTimeUnset
    case FLAT_OBSERVE_TYPE    => BiasTimeUnset
    case ARC_OBSERVE_TYPE     => BiasTimeEmpty
    case DARK_OBSERVE_TYPE    => BiasTimeEmpty
    case BIAS_OBSERVE_TYPE    => BiasTimeUnset
    case _                    => BiasTimeUnset
  }

  private def shutterStateObserveType(observeType: String): ShutterState = observeType match {
    case SCIENCE_OBSERVE_TYPE => UnsetShutter
    case FLAT_OBSERVE_TYPE    => UnsetShutter
    case ARC_OBSERVE_TYPE     => OpenShutter
    case DARK_OBSERVE_TYPE    => CloseShutter
    case BIAS_OBSERVE_TYPE    => CloseShutter
    case _                    => UnsetShutter
  }

  private def customROIs(config: Config): List[ROI] = {
    def attemptROI(i: Int): Option[ROI] =
      (for {
        xStart <- config.extract(INSTRUMENT_KEY / s"customROI${i}Xmin").as[java.lang.Integer].map(_.toInt)
        xRange <- config.extract(INSTRUMENT_KEY / s"customROI${i}Xrange").as[java.lang.Integer].map(_.toInt)
        yStart <- config.extract(INSTRUMENT_KEY / s"customROI${i}Ymin").as[java.lang.Integer].map(_.toInt)
        yRange <- config.extract(INSTRUMENT_KEY / s"customROI${i}Yrange").as[java.lang.Integer].map(_.toInt)
      } yield new ROI(xStart, yStart, xRange, yRange)).toOption

    val rois = for {
      i <- 1 to 5
    } yield attemptROI(i)
    rois.toList.flatten
  }

  def dcConfigFromSequenceConfig(config: Config): TrySeq[DCConfig] =
    (for {
      obsType      <- config.extract(OBSERVE_KEY / OBSERVE_TYPE_PROP).as[String]
      biasTime     <- \/.right(biasTimeObserveType(obsType))
      shutterState <- \/.right(shutterStateObserveType(obsType))
      exposureTime <- config.extract(OBSERVE_KEY / EXPOSURE_TIME_PROP).as[java.lang.Double].map(_.toDouble.seconds)
      ampReadMode  <- config.extract(AmpReadMode.KEY).as[AmpReadMode]
      gainChoice   <- config.extract(INSTRUMENT_KEY / AMP_GAIN_CHOICE_PROP).as[AmpGain]
      ampCount     <- config.extract(INSTRUMENT_KEY / AMP_COUNT_PROP).as[AmpCount]
      gainSetting  <- config.extract(INSTRUMENT_KEY / AMP_GAIN_SETTING_PROP).as[String].flatMap(s => s.parseDouble.disjunction.leftMap(_ => ConversionError(INSTRUMENT_KEY / AMP_GAIN_SETTING_PROP, "Bad Amp gain setting")))
      xBinning     <- config.extract(INSTRUMENT_KEY / CCD_X_BIN_PROP).as[Binning]
      yBinning     <- config.extract(INSTRUMENT_KEY / CCD_Y_BIN_PROP).as[Binning]
      builtInROI   <- config.extract(INSTRUMENT_KEY / BUILTIN_ROI_PROP).as[BuiltinROI]
      customROI = if (builtInROI == BuiltinROI.CUSTOM) customROIs(config) else Nil
      roi          <- RegionsOfInterest.fromOCS(builtInROI, customROI).leftMap(e => ContentError(SeqexecFailure.explain(e)))
    } yield
      DCConfig(exposureTime, biasTime, shutterState, CCDReadout(ampReadMode, gainChoice, ampCount, gainSetting), CCDBinning(xBinning, yBinning), roi))
        .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

 }
