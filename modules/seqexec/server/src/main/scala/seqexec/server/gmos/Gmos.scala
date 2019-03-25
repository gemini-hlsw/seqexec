// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.data.{EitherT, Reader}
import cats.implicits._
import cats.effect.IO
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.GmosCommonType._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon._
import edu.gemini.spModel.obscomp.InstConstants.{EXPOSURE_TIME_PROP, _}
import edu.gemini.spModel.seqcomp.SeqConfigNames.{INSTRUMENT_KEY, OBSERVE_KEY}
import java.lang.{Double => JDouble, Integer => JInt}

import edu.gemini.spModel.gemini.gmos.GmosCommonType
import gem.enum.LightSinkName
import mouse.all._
import org.log4s.{Logger, getLogger}

import scala.concurrent.duration._
import seqexec.model.dhs.ImageFileId
import seqexec.server.ConfigUtilOps.{ContentError, ConversionError, _}
import seqexec.server.gmos.Gmos.SiteSpecifics
import seqexec.server.gmos.GmosController.Config._
import seqexec.server.gmos.GmosController.SiteDependentTypes
import seqexec.server.keywords.{DhsInstrument, KeywordsClient}
import seqexec.server._
import squants.space.Length
import squants.{Seconds, Time}
import squants.space.LengthConversions._

abstract class Gmos[T<:GmosController.SiteDependentTypes](controller: GmosController[T], ss: SiteSpecifics[T])(configTypes: GmosController.Config[T]) extends DhsInstrument[IO] with InstrumentSystem[IO] {
  import Gmos._
  import InstrumentSystem._

  override def sfName(config: Config): LightSinkName = LightSinkName.Gmos

  override val contributorName: String = "gmosdc"

  override val keywordsClient: KeywordsClient[IO] = this

  override val observeControl: InstrumentSystem.ObserveControl = OpticControl(
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

  private def calcDisperser(disp: T#Disperser, order: Option[DisperserOrder], wl: Option[Length])
  : Either[ConfigUtilOps.ExtractFailure, configTypes.GmosDisperser] =
    if(configTypes.isMirror(disp))
      configTypes.GmosDisperser.Mirror.asRight[ConfigUtilOps.ExtractFailure]
    else order.map{o =>
      if(o === GmosCommonType.Order.ZERO)
        configTypes.GmosDisperser.Order0(disp).asRight[ConfigUtilOps.ExtractFailure]
      else wl.map(w => configTypes.GmosDisperser.OrderN(disp, o, w)
        .asRight[ConfigUtilOps.ExtractFailure]).getOrElse(
          ConfigUtilOps.ContentError(s"Disperser order ${o.displayValue} is missing a wavelength.")
          .asLeft
        )
    }.getOrElse(ConfigUtilOps.ContentError(s"Disperser is missing an order.").asLeft)

  private def ccConfigFromSequenceConfig(config: Config): TrySeq[configTypes.CCConfig] =
    (for {
      filter           <- ss.extractFilter(config)
      disp             <- ss.extractDisperser(config)
      disperserOrder   =  config.extractAs[DisperserOrder](INSTRUMENT_KEY / DISPERSER_ORDER_PROP)
      disperserLambda  =  config.extractAs[JDouble](INSTRUMENT_KEY / DISPERSER_LAMBDA_PROP).map(_.toDouble.nanometers)
      fpuName          =  ss.extractFPU(config)
      fpuMask          =  config.extractAs[String](INSTRUMENT_KEY / FPU_MASK_PROP)
      fpu              <- config.extractAs[FPUnitMode](INSTRUMENT_KEY / FPU_MODE_PROP).map(fpuFromFPUnit(fpuName.toOption, fpuMask.toOption))
      stageMode        <- ss.extractStageMode(config)
      dtax             <- config.extractAs[DTAX](INSTRUMENT_KEY / DTAX_OFFSET_PROP)
      adc              <- config.extractAs[ADC](INSTRUMENT_KEY / ADC_PROP)
      electronicOffset =  config.extractAs[UseElectronicOffset](INSTRUMENT_KEY / USE_ELECTRONIC_OFFSETTING_PROP)
      disperser        <- calcDisperser(disp, disperserOrder.toOption, disperserLambda.toOption)
    } yield configTypes.CCConfig(filter, disperser, fpu, stageMode, dtax, adc, electronicOffset.toOption)).leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  private def fromSequenceConfig(config: Config): SeqAction[GmosController.GmosConfig[T]] = EitherT( IO ( for {
      cc <- ccConfigFromSequenceConfig(config)
      dc <- dcConfigFromSequenceConfig(config)
    } yield new GmosController.GmosConfig[T](configTypes)(cc, dc)
  ) )

  override def observe(config: Config): SeqObserve[ImageFileId, ObserveCommand.Result] =
    Reader { fileId =>
      SeqActionF.liftF(calcObserveTime(config)).flatMap {
        controller.observe(fileId, _)
      }
    }

  override def notifyObserveEnd: SeqAction[Unit] = controller.endObserve

  override def notifyObserveStart: SeqAction[Unit] = SeqAction.void

  override def configure(config: Config): SeqAction[ConfigResult[IO]] =
    fromSequenceConfig(config).flatMap(controller.applyConfig).as(ConfigResult(this))

  override def calcObserveTime(config: Config): IO[Time] =
    IO(config.extractAs[JDouble](OBSERVE_KEY / EXPOSURE_TIME_PROP)
      .map(v => Seconds(v.toDouble)).getOrElse(Seconds(10000)))

  override def observeProgress(total: Time, elapsed: ElapsedTime): fs2.Stream[IO, Progress] = controller
    .observeProgress(total, elapsed)
}

object Gmos {
  val name: String = INSTRUMENT_NAME_PROP

  trait SiteSpecifics[T<:SiteDependentTypes] {
    def extractFilter(config: Config): Either[ExtractFailure, T#Filter]

    def extractDisperser(config: Config): Either[ExtractFailure, T#Disperser]

    def extractFPU(config: Config): Either[ExtractFailure, T#FPU]

    def extractStageMode(config: Config): Either[ExtractFailure, T#GmosStageMode]

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
        xStart <- config.extractAs[JInt](INSTRUMENT_KEY / s"customROI${i}Xmin").map(_.toInt)
        xRange <- config.extractAs[JInt](INSTRUMENT_KEY / s"customROI${i}Xrange").map(_.toInt)
        yStart <- config.extractAs[JInt](INSTRUMENT_KEY / s"customROI${i}Ymin").map(_.toInt)
        yRange <- config.extractAs[JInt](INSTRUMENT_KEY / s"customROI${i}Yrange").map(_.toInt)
      } yield new ROI(xStart, yStart, xRange, yRange)).toOption

    val rois = for {
      i <- 1 to 5
    } yield attemptROI(i)
    rois.toList.collect { case Some(x) => x }
  }

  def dcConfigFromSequenceConfig(config: Config): TrySeq[DCConfig] =
    (for {
      obsType      <- config.extractAs[String](OBSERVE_KEY / OBSERVE_TYPE_PROP)
      biasTime     <- biasTimeObserveType(obsType).asRight
      shutterState <- shutterStateObserveType(obsType).asRight
      exposureTime <- config.extractAs[JDouble](OBSERVE_KEY / EXPOSURE_TIME_PROP).map(_.toDouble.seconds)
      ampReadMode  <- config.extractAs[AmpReadMode](AmpReadMode.KEY)
      gainChoice   <- config.extractAs[AmpGain](INSTRUMENT_KEY / AMP_GAIN_CHOICE_PROP)
      ampCount     <- config.extractAs[AmpCount](INSTRUMENT_KEY / AMP_COUNT_PROP)
      gainSetting  <- config.extractAs[String](INSTRUMENT_KEY / AMP_GAIN_SETTING_PROP).flatMap(s => s.parseDouble.leftMap(_ => ConversionError(INSTRUMENT_KEY / AMP_GAIN_SETTING_PROP, "Bad Amp gain setting")))
      xBinning     <- config.extractAs[Binning](INSTRUMENT_KEY / CCD_X_BIN_PROP)
      yBinning     <- config.extractAs[Binning](INSTRUMENT_KEY / CCD_Y_BIN_PROP)
      builtInROI   <- config.extractAs[BuiltinROI](INSTRUMENT_KEY / BUILTIN_ROI_PROP)
      customROI = if (builtInROI === BuiltinROI.CUSTOM) customROIs(config) else Nil
      roi          <- RegionsOfInterest.fromOCS(builtInROI, customROI).leftMap(e => ContentError(SeqexecFailure.explain(e)))
    } yield
      DCConfig(exposureTime, biasTime, shutterState, CCDReadout(ampReadMode, gainChoice, ampCount, gainSetting), CCDBinning(xBinning, yBinning), roi))
        .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

 }
