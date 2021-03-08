// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import java.lang.{ Double => JDouble }
import java.lang.{ Integer => JInt }

import scala.concurrent.duration._

import cats._
import cats.data.EitherT
import cats.data.Kleisli
import cats.effect.Concurrent
import cats.effect.Sync
import cats.effect.Timer
import cats.effect.concurrent.Ref
import cats.syntax.all._
import edu.gemini.spModel.config2.ItemKey
import edu.gemini.spModel.gemini.gmos.GmosCommonType
import edu.gemini.spModel.gemini.gmos.GmosCommonType._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon._
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.obscomp.InstConstants.EXPOSURE_TIME_PROP
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import org.typelevel.log4cats.Logger
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.syntax.string._
import seqexec.model.GmosParameters._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Guiding
import seqexec.model.`enum`.Instrument
import seqexec.model.enum.NodAndShuffleStage
import seqexec.model.enum.NodAndShuffleStage._
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigUtilOps.ContentError
import seqexec.server.ConfigUtilOps.ConversionError
import seqexec.server.ConfigUtilOps._
import seqexec.server._
import seqexec.server.gmos.Gmos.SiteSpecifics
import seqexec.server.gmos.GmosController.Config.NSConfig
import seqexec.server.gmos.GmosController.Config._
import seqexec.server.gmos.GmosController.SiteDependentTypes
import seqexec.server.keywords.DhsInstrument
import seqexec.server.keywords.KeywordsClient
import shapeless.tag
import squants.Seconds
import squants.Time
import squants.space.Length
import squants.space.LengthConversions._

abstract class Gmos[F[_]: Concurrent: Timer: Logger, T <: GmosController.SiteDependentTypes](
  val controller: GmosController[F, T],
  ss:             SiteSpecifics[T],
  nsCmdR:         Ref[F, Option[NSObserveCommand]]
)(configTypes:    GmosController.Config[T])
    extends DhsInstrument[F]
    with InstrumentSystem[F] {
  import Gmos._
  import InstrumentSystem._

  override val contributorName: String = "gmosdc"

  override val keywordsClient: KeywordsClient[F] = this

  val nsCmdRef: Ref[F, Option[NSObserveCommand]] = nsCmdR

  val nsCount: F[Int] = controller.nsCount

  override def observeTimeout: FiniteDuration = 110.seconds

  override def observeControl(config: CleanConfig): InstrumentSystem.CompleteControl[F] =
    if (isNodAndShuffle(config))
      CompleteControl(
        StopObserveCmd(stopNS),
        AbortObserveCmd(abortNS),
        PauseObserveCmd(pauseNS),
        ContinuePausedCmd(controller.resumePaused),
        StopPausedCmd(controller.stopPaused),
        AbortPausedCmd(controller.abortPaused)
      )
    else
      CompleteControl(
        StopObserveCmd(_ => controller.stopObserve),
        AbortObserveCmd(controller.abortObserve),
        PauseObserveCmd(_ => controller.pauseObserve),
        ContinuePausedCmd(controller.resumePaused),
        StopPausedCmd(controller.stopPaused),
        AbortPausedCmd(controller.abortPaused)
      )

  private def stopNS(gracefully: Boolean): F[Unit] =
    if (gracefully)
      nsCmdRef.set(NSObserveCommand.StopGracefully.some)
    else
      nsCmdRef.set(NSObserveCommand.StopImmediately.some) *> controller.stopObserve

  private def abortNS: F[Unit] =
    nsCmdRef.set(NSObserveCommand.AbortImmediately.some) *> controller.abortObserve

  private def pauseNS(gracefully: Boolean): F[Unit] =
    if (gracefully)
      nsCmdRef.set(NSObserveCommand.PauseGracefully.some)
    else
      nsCmdRef.set(NSObserveCommand.PauseImmediately.some) *> controller.pauseObserve

  protected def fpuFromFPUnit(
    n:            Option[T#FPU],
    m:            Option[String],
    isCustomMask: Boolean
  ): GmosFPU =
    if (!isCustomMask)
      configTypes.BuiltInFPU(n.getOrElse(ss.fpuDefault))
    else
      (m, n) match {
        case (Some(u), _) => GmosController.Config.CustomMaskFPU(u)
        case _            => GmosController.Config.UnknownFPU
      }

  private def calcDisperser(
    disp:  T#Disperser,
    order: Option[DisperserOrder],
    wl:    Option[Length]
  ): Either[ConfigUtilOps.ExtractFailure, configTypes.GmosDisperser] =
    if (configTypes.isMirror(disp)) {
      configTypes.GmosDisperser.Mirror.asRight
    } else {
      // Workaround for missing order: Use order 1 as default
      val o = order.getOrElse(GmosCommonType.Order.ONE)

      if (o === GmosCommonType.Order.ZERO)
        configTypes.GmosDisperser.Order0(disp).asRight
      else
        wl.map(w => configTypes.GmosDisperser.OrderN(disp, o, w).asRight)
          .getOrElse(
            ConfigUtilOps
              .ContentError(s"Disperser order ${o.displayValue} is missing a wavelength.")
              .asLeft
          )
    }

  private def ccConfigFromSequenceConfig(
    config: CleanConfig
  ): Either[SeqexecFailure, configTypes.CCConfig] =
    (for {
      filter          <- ss.extractFilter(config)
      disp            <- ss.extractDisperser(config)
      disperserOrder   = config.extractInstAs[DisperserOrder](DISPERSER_ORDER_PROP)
      disperserLambda  =
        config.extractInstAs[JDouble](DISPERSER_LAMBDA_PROP).map(_.toDouble.nanometers)
      fpuName          = ss.extractFPU(config)
      customMask       = ss.isCustomFPU(config)
      fpuMask          = config.extractInstAs[String](FPU_MASK_PROP)
      fpu              = fpuFromFPUnit(fpuName.toOption, fpuMask.toOption, customMask)
      stageMode       <- ss.extractStageMode(config)
      dtax            <- config.extractInstAs[DTAX](DTAX_OFFSET_PROP)
      adc             <- config.extractInstAs[ADC](ADC_PROP)
      electronicOffset = Gmos.ccElectronicOffset(config)
      disperser       <- calcDisperser(disp, disperserOrder.toOption, disperserLambda.toOption)
      obsType         <- config.extractObsAs[String](OBSERVE_TYPE_PROP)
      isDarkOrBias     = List(DARK_OBSERVE_TYPE, BIAS_OBSERVE_TYPE).exists(_ === obsType)
    } yield configTypes.CCConfig(filter,
                                 disperser,
                                 fpu,
                                 stageMode,
                                 dtax,
                                 adc,
                                 electronicOffset,
                                 isDarkOrBias
    ))
      .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  private def fromSequenceConfig(
    config: CleanConfig
  ): Either[SeqexecFailure, GmosController.GmosConfig[T]] =
    for {
      cc <- ccConfigFromSequenceConfig(config)
      ns <- Gmos.nsConfig(config)
      dc <- dcConfigFromSequenceConfig(config, ns)
    } yield new GmosController.GmosConfig[T](configTypes)(cc, dc, ns)

  override def observe(config: CleanConfig): Kleisli[F, ImageFileId, ObserveCommandResult] =
    Kleisli { fileId =>
      calcObserveTime(config).flatMap { x =>
        controller.observe(fileId, x)
      }
    }

  override def instrumentActions(config: CleanConfig): InstrumentActions[F] =
    new GmosInstrumentActions(this, config)

  override def notifyObserveEnd: F[Unit] =
    controller.endObserve

  override def notifyObserveStart: F[Unit] = Applicative[F].unit

  override def configure(config: CleanConfig): F[ConfigResult[F]] =
    EitherT
      .fromEither[F](fromSequenceConfig(config))
      .widenRethrowT
      .flatMap(controller.applyConfig)
      .as(ConfigResult(this))

  override def calcObserveTime(config: CleanConfig): F[Time] =
    (Gmos.expTimeF[F](config), Gmos.nsConfigF[F](config)).mapN { (v, ns) =>
      v / ns.exposureDivider.toDouble
    }

  override def observeProgress(total: Time, elapsed: ElapsedTime): fs2.Stream[F, Progress] =
    controller
      .observeProgress(total, elapsed)

}

object Gmos {
  val name: String = INSTRUMENT_NAME_PROP

  def rowsToShuffle(stage: NodAndShuffleStage, rows: Int): Int =
    if (stage === StageA) 0 else rows

  trait SiteSpecifics[T <: SiteDependentTypes] {
    final val FPU_CUSTOM_MASK = "fpuCustomMask"

    def extractFilter(config: CleanConfig): Either[ExtractFailure, T#Filter]

    def extractDisperser(config: CleanConfig): Either[ExtractFailure, T#Disperser]

    def extractFPU(config: CleanConfig): Either[ExtractFailure, T#FPU]

    def extractStageMode(config: CleanConfig): Either[ExtractFailure, T#GmosStageMode]

    val fpuDefault: T#FPU

    def extractCustomFPU(config: CleanConfig): Either[ConfigUtilOps.ExtractFailure, String] =
      config.extractInstAs[String](FPU_CUSTOM_MASK)

    def isCustomFPU(config: CleanConfig): Boolean
  }

  val NSKey: ItemKey = INSTRUMENT_KEY / USE_NS_PROP

  def isNodAndShuffle(config: CleanConfig): Boolean =
    config
      .extractAs[java.lang.Boolean](NSKey)
      .map(_.booleanValue())
      .getOrElse(false)

  def ccElectronicOffset(config: CleanConfig): ElectronicOffset =
    ElectronicOffset.fromBoolean(
      config
        .extractInstAs[java.lang.Boolean](USE_ELECTRONIC_OFFSETTING_PROP)
        .map(_.booleanValue())
        .getOrElse(
          false
        ) // We should always set electronic offset to false unless explicitly enabled
    )

  private def configToAngle(s: String): Either[ExtractFailure, Angle] =
    s.parseDoubleOption
      .toRight(ContentError("Invalid offset value"))
      .map(Angle.fromDoubleArcseconds)

  private def extractGuiding(config: CleanConfig, k: ItemKey): Either[ExtractFailure, Guiding] =
    config
      .extractAs[StandardGuideOptions.Value](k)
      .flatMap(r => Guiding.fromString(r.toString).toRight(KeyNotFound(k)))
      .orElse {
        config.extractAs[String](k).flatMap(Guiding.fromString(_).toRight(KeyNotFound(k)))
      }

  private def nsPosition(config: CleanConfig, sc: Int): Either[ExtractFailure, Vector[NSPosition]] =
    NodAndShuffleStage.NSStageEnumerated.all
      .slice(0, sc)
      .map { s =>
        for {
          p <- config
                 .extractInstAs[String](s"nsBeam${s.symbol.name}-p")
                 .flatMap(configToAngle)
                 .map(Offset.P.apply)
          q <- config
                 .extractInstAs[String](s"nsBeam${s.symbol.name}-q")
                 .flatMap(configToAngle)
                 .map(Offset.Q.apply)
          k  = INSTRUMENT_KEY / s"nsBeam${s.symbol.name}-guideWithOIWFS"
          g <- extractGuiding(config, k)
        } yield NSPosition(s, Offset(p, q), g)
      }
      .toVector
      .sequence

  def nodAndShuffle(config: CleanConfig): Either[ExtractFailure, NSConfig.NodAndShuffle] =
    for {
      cycles <- config.extractInstAs[JInt](NUM_NS_CYCLES_PROP).map(_.toInt)
      rows   <- config.extractInstAs[JInt](DETECTOR_ROWS_PROP).map(_.toInt)
      sc     <- config.extractInstAs[JInt](NS_STEP_COUNT_PROP_NAME)
      pos    <- nsPosition(config, sc)
    } yield NSConfig.NodAndShuffle(tag[NsCyclesI][Int](cycles),
                                   tag[NsRowsI][Int](rows),
                                   pos,
                                   expTime(config)
    )

  def nsConfig(config: CleanConfig): Either[SeqexecFailure, NSConfig] =
    (for {
      useNS <- config.extractInstAs[java.lang.Boolean](USE_NS_PROP)
      ns    <- if (useNS) nodAndShuffle(config) else NSConfig.NoNodAndShuffle.asRight
    } yield ns).leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  def nsConfigF[F[_]: ApplicativeError[*[_], Throwable]](config: CleanConfig): F[NSConfig] =
    ApplicativeError[F, Throwable]
      .catchNonFatal(
        nsConfig(config).getOrElse(NSConfig.NoNodAndShuffle)
      )

  def expTime(config: CleanConfig): Time =
    config
      .extractObsAs[JDouble](EXPOSURE_TIME_PROP)
      .map(v => Seconds(v.toDouble))
      .getOrElse(Seconds(10000))

  def expTimeF[F[_]: ApplicativeError[*[_], Throwable]](config: CleanConfig): F[Time] =
    ApplicativeError[F, Throwable]
      .catchNonFatal(
        expTime(config)
      )

  private def shutterStateObserveType(observeType: String): ShutterState = observeType match {
    case SCIENCE_OBSERVE_TYPE => ShutterState.OpenShutter
    case FLAT_OBSERVE_TYPE    => ShutterState.OpenShutter
    case ARC_OBSERVE_TYPE     => ShutterState.OpenShutter
    case DARK_OBSERVE_TYPE    => ShutterState.CloseShutter
    case BIAS_OBSERVE_TYPE    => ShutterState.CloseShutter
    case _                    => ShutterState.UnsetShutter
  }

  private def customROIs(config: CleanConfig): List[ROI] = {
    def attemptROI(i: Int): Option[ROI] =
      (for {
        xStart <- config.extractInstAs[JInt](s"customROI${i}Xmin").map(_.toInt)
        xRange <- config.extractInstAs[JInt](s"customROI${i}Xrange").map(_.toInt)
        yStart <- config.extractInstAs[JInt](s"customROI${i}Ymin").map(_.toInt)
        yRange <- config.extractInstAs[JInt](s"customROI${i}Yrange").map(_.toInt)
      } yield new ROI(xStart, yStart, xRange, yRange)).toOption

    val rois = for {
      i <- 1 to 5
    } yield attemptROI(i)
    rois.toList.flattenOption
  }

  private def toGain(s: String): Either[ExtractFailure, Double] =
    s.parseDoubleOption
      .toRight(ConversionError(INSTRUMENT_KEY / AMP_GAIN_SETTING_PROP, "Bad Amp gain setting"))

  private def exposureTime(
    config:   CleanConfig,
    nsConfig: NSConfig
  ): Either[ExtractFailure, FiniteDuration] =
    config.extractObsAs[JDouble](EXPOSURE_TIME_PROP).map { e =>
      (e / nsConfig.exposureDivider).seconds
    }

  def dcConfigFromSequenceConfig(
    config:   CleanConfig,
    nsConfig: NSConfig
  ): Either[SeqexecFailure, DCConfig] =
    (for {
      obsType      <- config.extractObsAs[String](OBSERVE_TYPE_PROP)
      shutterState <- shutterStateObserveType(obsType).asRight
      exposureTime <- exposureTime(config, nsConfig)
      ampReadMode  <- config.extractAs[AmpReadMode](AmpReadMode.KEY)
      gainChoice   <- config.extractInstAs[AmpGain](AMP_GAIN_CHOICE_PROP)
      ampCount     <- config.extractInstAs[AmpCount](AMP_COUNT_PROP)
      gainSetting  <- config.extractInstAs[String](AMP_GAIN_SETTING_PROP).flatMap(toGain)
      xBinning     <- config.extractInstAs[Binning](CCD_X_BIN_PROP)
      yBinning     <- config.extractInstAs[Binning](CCD_Y_BIN_PROP)
      roi          <- extractROIs(config)
    } yield DCConfig(exposureTime,
                     shutterState,
                     CCDReadout(ampReadMode, gainChoice, ampCount, gainSetting),
                     CCDBinning(xBinning, yBinning),
                     roi
    ))
      .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  def nsCmdRef[F[_]: Sync]: F[Ref[F, Option[NSObserveCommand]]] = Ref.of(none)

  def extractROIs(config: CleanConfig): Either[ExtractFailure, RegionsOfInterest] = for {
    builtInROI <- config.extractInstAs[BuiltinROI](BUILTIN_ROI_PROP)
    customROI   = if (builtInROI === BuiltinROI.CUSTOM) customROIs(config) else Nil
    roi        <- RegionsOfInterest
                    .fromOCS(builtInROI, customROI)
                    .leftMap(e => ContentError(SeqexecFailure.explain(e)))
  } yield roi

  def calcStepType(
    instrument: Instrument,
    config:     CleanConfig,
    isNightSeq: Boolean
  ): Either[SeqexecFailure, StepType] = {
    val stdType = SequenceConfiguration.calcStepType(config, isNightSeq)
    if (Gmos.isNodAndShuffle(config)) {
      stdType.flatMap {
        case StepType.ExclusiveDarkOrBias(_) => StepType.DarkOrBiasNS(instrument).asRight
        case StepType.CelestialObject(_)     => StepType.NodAndShuffle(instrument).asRight
        case st                              => SeqexecFailure.Unexpected(s"N&S is not supported for steps of type $st").asLeft
      }
    } else {
      stdType
    }
  }

}
