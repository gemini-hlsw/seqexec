// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gmos

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.ConfigUtilOps.{ContentError, ConversionError, _}
import edu.gemini.seqexec.server.gmos.Gmos.SiteSpecifics
import edu.gemini.seqexec.server.gmos.GmosController.Config._
import edu.gemini.seqexec.server.gmos.GmosController.SiteDependentTypes
import edu.gemini.seqexec.server.{ConfigResult, ConfigUtilOps, InstrumentSystem, ObserveCommand, SeqAction, SeqObserve, SeqexecFailure, TrySeq}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.GmosCommonType._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon._
import edu.gemini.spModel.obscomp.InstConstants.{EXPOSURE_TIME_PROP, _}
import edu.gemini.spModel.seqcomp.SeqConfigNames.{INSTRUMENT_KEY, OBSERVE_KEY}
import org.log4s.{Logger, getLogger}
import squants.{Time, Seconds}
import squants.space.LengthConversions._

import scala.concurrent.duration._
import scalaz.concurrent.Task
import scalaz.syntax.std.string._
import scalaz.syntax.equal._
import scalaz.{EitherT, Reader, \/}

/**
  * Created by jluhrs on 8/3/17.
  */
abstract class Gmos[T<:GmosController.SiteDependentTypes](controller: GmosController[T], ss: SiteSpecifics[T])(configTypes: GmosController.Config[T]) extends InstrumentSystem {
  import Gmos._
  import InstrumentSystem._

  override val sfName: String = "gmos"

  override val contributorName: String = "gmosdc"

  override val observeControl: InstrumentSystem.ObserveControl = Controllable(
    StopObserveCmd(controller.stopObserve),
    AbortObserveCmd(controller.abortObserve),
    PauseObserveCmd(controller.pauseObserve),
    ContinuePausedCmd{t: Time => controller.resumePaused(t)},
    StopPausedCmd(controller.stopPaused),
    AbortPausedCmd(controller.abortPaused)
  )

  val Log: Logger = getLogger

  protected def fpuFromFPUnit(n: Option[T#FPU], m: Option[String])(fpu: FPUnitMode): GmosFPU = fpu match {
    case FPUnitMode.BUILTIN     => configTypes.BuiltInFPU(n.getOrElse(ss.fpuDefault))
    case FPUnitMode.CUSTOM_MASK => m match {
      case Some(u) => GmosController.Config.CustomMaskFPU(u)
      case _       => GmosController.Config.UnknownFPU
    }
  }

  private def ccConfigFromSequenceConfig(config: Config): TrySeq[configTypes.CCConfig] =
    (for {
      filter           <- ss.extractFilter(config)
      disp             <- ss.extractDisperser(config)
      disperserOrder   =  config.extract(INSTRUMENT_KEY / DISPERSER_ORDER_PROP).as[DisperserOrder]
      disperserLambda  =  config.extract(INSTRUMENT_KEY / DISPERSER_LAMBDA_PROP).as[java.lang.Double].map(_.toDouble.nanometers)
      fpuName          =  ss.extractFPU(config)
      fpuMask          =  config.extract(INSTRUMENT_KEY / FPU_MASK_PROP).as[String]
      fpu              <- config.extract(INSTRUMENT_KEY / FPU_MODE_PROP).as[FPUnitMode].map(fpuFromFPUnit(fpuName.toOption, fpuMask.toOption))
      stageMode        <- ss.extractStageMode(config)
      dtax             <- config.extract(INSTRUMENT_KEY / DTAX_OFFSET_PROP).as[DTAX]
      adc              <- config.extract(INSTRUMENT_KEY / ADC_PROP).as[ADC]
      electronicOffset =  config.extract(INSTRUMENT_KEY / USE_ELECTRONIC_OFFSETTING_PROP).as[UseElectronicOffset]
      disperser = configTypes.GmosDisperser(disp, disperserOrder.toOption, disperserLambda.toOption)
    } yield configTypes.CCConfig(filter, disperser, fpu, stageMode, dtax, adc, electronicOffset.toOption)).leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  private def fromSequenceConfig(config: Config): SeqAction[GmosController.GmosConfig[T]] = EitherT( Task ( for {
      cc <- ccConfigFromSequenceConfig(config)
      dc <- dcConfigFromSequenceConfig(config)
    } yield new GmosController.GmosConfig[T](configTypes)(cc, dc)
  ) )

  override def observe(config: Config): SeqObserve[ImageFileId, ObserveCommand.Result] = Reader {
    fileId => controller.observe(fileId, calcObserveTime(config))
  }

  override def notifyObserveEnd: SeqAction[Unit] = controller.endObserve

  override def configure(config: Config): SeqAction[ConfigResult] =
    fromSequenceConfig(config).flatMap(controller.applyConfig).map(_ => ConfigResult(this))

  override def calcObserveTime(config: Config): Time =
    config.extract(OBSERVE_KEY / EXPOSURE_TIME_PROP).as[java.lang.Double].map(v => Seconds(v.toDouble)).getOrElse(Seconds(10000))
}

object Gmos {
  val name: String = INSTRUMENT_NAME_PROP

  trait SiteSpecifics[T<:SiteDependentTypes] {
    def extractFilter(config: Config): ExtractFailure\/T#Filter

    def extractDisperser(config: Config): ExtractFailure\/T#Disperser

    def extractFPU(config: Config): ExtractFailure\/T#FPU

    def extractStageMode(config: Config): ExtractFailure\/T#GmosStageMode

    val fpuDefault: T#FPU
  }

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
    case SCIENCE_OBSERVE_TYPE => OpenShutter
    case FLAT_OBSERVE_TYPE    => OpenShutter
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
    rois.toList.collect { case Some(x) => x }
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
      customROI = if (builtInROI === BuiltinROI.CUSTOM) customROIs(config) else Nil
      roi          <- RegionsOfInterest.fromOCS(builtInROI, customROI).leftMap(e => ContentError(SeqexecFailure.explain(e)))
    } yield
      DCConfig(exposureTime, biasTime, shutterState, CCDReadout(ampReadMode, gainChoice, ampCount, gainSetting), CCDBinning(xBinning, yBinning), roi))
        .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

 }
