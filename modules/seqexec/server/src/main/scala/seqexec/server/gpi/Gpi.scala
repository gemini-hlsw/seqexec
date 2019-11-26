// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats._
import cats.data.EitherT
import cats.data.Kleisli
import cats.effect.{Concurrent, Timer}
import cats.implicits._
import edu.gemini.spModel.gemini.gpi.Gpi._
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import edu.gemini.spModel.obscomp.InstConstants
import edu.gemini.spModel.obsclass.ObsClass
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import java.lang.{Boolean => JBoolean}
import java.lang.{Double => JDouble}
import java.lang.{Integer => JInt}

import gem.enum.GpiReadMode
import gem.enum.LightSinkName
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Instrument
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.ConfigUtilOps._
import seqexec.server._
import seqexec.server.CleanConfig.extractItem
import seqexec.server.keywords.GdsClient
import seqexec.server.keywords.GdsInstrument
import seqexec.server.keywords.KeywordsClient

import scala.concurrent.duration._
import squants.Time
import squants.time.Milliseconds
import squants.time.Seconds
import squants.time.Time

final case class Gpi[F[_]: Timer: Logger: Concurrent](controller: GpiController[F])
    extends GdsInstrument[F]
    with InstrumentSystem[F] {

  override val gdsClient: GdsClient[F] = controller.gdsClient

  // Taken from the gpi isd
  val readoutOverhead: Time  = Seconds(4)
  val writeOverhead: Time    = Seconds(2)
  val perCoaddOverhead: Time = Seconds(2.7)
  val timeoutTolerance: Time = Seconds(30)

  override val keywordsClient: KeywordsClient[F] = this

  override val resource: Instrument = Instrument.Gpi

  override def sfName(config: CleanConfig): LightSinkName = LightSinkName.Gpi

  override val contributorName: String = "gpi"

  override def calcStepType(config: CleanConfig, isNightSeq: Boolean): Either[SeqexecFailure, StepType] =
    if (Gpi.isAlignAndCalib(config)) {
      StepType.AlignAndCalib.asRight
    } else {
      SequenceConfiguration.calcStepType(config, isNightSeq)
    }

  override def observeControl(config: CleanConfig): InstrumentSystem.ObserveControl[F] =
    InstrumentSystem.Uncontrollable

  override def observe(
    config: CleanConfig
  ): Kleisli[F, ImageFileId, ObserveCommandResult] =
    Kleisli { fileId =>
      calcObserveTime(config).flatMap { ot =>
        controller
          .observe(fileId, timeoutTolerance + ot)
          .as(ObserveCommandResult.Success: ObserveCommandResult)
      }
    }

  override def configure(config: CleanConfig): F[ConfigResult[F]] =
    if (Gpi.isAlignAndCalib(config)) {
      controller.alignAndCalib.as(ConfigResult(this))
    } else {
      Gpi
        .fromSequenceConfig[F](config)
        .flatMap(controller.applyConfig)
        .as(ConfigResult(this))
    }

  override def notifyObserveEnd: F[Unit] =
    controller.endObserve

  override def notifyObserveStart: F[Unit] = Applicative[F].unit

  override def calcObserveTime(config: CleanConfig): F[Time] = MonadError[F, Throwable].catchNonFatal {
    val obsTime =
      for {
        exp <- config.extractObsAs[JDouble](EXPOSURE_TIME_PROP)
        coa <- config.extractObsAs[JInt](COADDS_PROP).map(_.toInt)
      } yield
        (Seconds(exp.toDouble) + perCoaddOverhead) * coa.toDouble + readoutOverhead + writeOverhead
    obsTime.getOrElse(Milliseconds(100))
  }

  override def observeProgress(
    total:   Time,
    elapsed: InstrumentSystem.ElapsedTime
  ): Stream[F, Progress] =
    ProgressUtil.obsCountdown[F](total, elapsed.self)

  override def instrumentActions(config: CleanConfig): InstrumentActions[F] =
    new GpiInstrumentActions[F]

}

object Gpi {
  val name: String = INSTRUMENT_NAME_PROP

  private def gpiAoFlags(config: CleanConfig): Either[ExtractFailure, AOFlags] =
    for {
      useAo      <- config.extractInstAs[JBoolean](USE_AO_PROP)
      useCal     <- config.extractInstAs[JBoolean](USE_CAL_PROP)
      aoOptimize <- config.extractInstAs[JBoolean](AO_OPTIMIZE_PROP)
      alignFpm   <- config.extractInstAs[JBoolean](ALIGN_FPM_PINHOLE_BIAS_PROP)
      magH       <- config.extractInstAs[JDouble](MAG_H_PROP)
      magI       <- config.extractInstAs[JDouble](MAG_I_PROP)
    } yield AOFlags(useAo, useCal, aoOptimize, alignFpm, magH, magI)

  private def gpiASU(
    config: CleanConfig
  ): Either[ExtractFailure, ArtificialSources] =
    for {
      ir          <- config.extractInstAs[ArtificialSource](IR_LASER_LAMP_PROP)
      vis         <- config.extractInstAs[ArtificialSource](VISIBLE_LASER_LAMP_PROP)
      sc          <- config.extractInstAs[ArtificialSource](SUPER_CONTINUUM_LAMP_PROP)
      attenuation <- config.extractInstAs[JDouble](ARTIFICIAL_SOURCE_ATTENUATION_PROP)
                       .map(_.toDouble)
    } yield ArtificialSources(ir, vis, sc, attenuation)

  private def gpiShutters(config: CleanConfig): Either[ExtractFailure, Shutters] =
    for {
      entrance     <- config.extractInstAs[Shutter](ENTRANCE_SHUTTER_PROP)
      calEntrance  <- config.extractInstAs[Shutter](CAL_ENTRANCE_SHUTTER_PROP)
      scienceArm   <- config.extractInstAs[Shutter](SCIENCE_ARM_SHUTTER_PROP)
      referenceArm <- config.extractInstAs[Shutter](REFERENCE_ARM_SHUTTER_PROP)
    } yield Shutters(entrance, calEntrance, scienceArm, referenceArm)

  private def gpiMode(config: CleanConfig)
    : Either[ExtractFailure, Either[ObservingMode, NonStandardModeParams]] =
    config
      .extractInstAs[ObservingMode](OBSERVING_MODE_PROP)
      .flatMap { mode =>
        if (mode === ObservingMode.NONSTANDARD) {
          for {
            apodizer <- config.extractInstAs[Apodizer](APODIZER_PROP)
            fpm      <- config.extractInstAs[FPM](FPM_PROP)
            lyot     <- config.extractInstAs[Lyot](LYOT_PROP)
            filter   <- config.extractInstAs[Filter](FILTER_PROP)
          } yield NonStandardModeParams(apodizer, fpm, lyot, filter).asRight
        } else mode.asLeft.asRight
      }

  val AcquisitionKey: String = ObsClass.ACQ.headerValue()

  // TODO wrap this on F to keep RT, It involves a large change upstream
  def isAlignAndCalib(config: CleanConfig): Boolean = {
    (config.extractInstAs[String](InstConstants.INSTRUMENT_NAME_PROP), config.extractObsAs[String](InstConstants.OBS_CLASS_PROP)).mapN {
      case (Gpi.name, "acq") => true
      case _                 => false
    }.getOrElse(false)
  }

  def gpiReadMode(config: CleanConfig): Either[ExtractFailure, GpiReadMode] =
    // FIXME: This turned out ugly because on the ocs this value is
    // stored in an class DetectorSamplingMode which is private
    // The OCS will be updated eventually but in the mean time we have to
    // resort to casting to Object and call toString
    // I beg your forgivness 🙏
    config.extractInstAs[Any](DETECTOR_SAMPLING_MODE_PROP)
      .flatMap{s =>
        GpiReadMode.fromLongName(s.toString)
        .toRight(
          ConversionError(OBSERVE_KEY / DETECTOR_SAMPLING_MODE_PROP,
          "Cannot read gpi Read mode"))}

  def gpiReadoutArea(config: CleanConfig): Either[ExtractFailure, ReadoutArea] =
    (for {
      startX <- config.extractInstAs[JInt](DETECTOR_STARTX_PROP)
      startY <- config.extractInstAs[JInt](DETECTOR_STARTY_PROP)
      endX   <- config.extractInstAs[JInt](DETECTOR_ENDX_PROP)
      endY   <- config.extractInstAs[JInt](DETECTOR_ENDY_PROP)
    } yield
      ReadoutArea.fromValues(startX, startY, endX, endY)
        .toRight(
          ConversionError(OBSERVE_KEY / DETECTOR_STARTX_PROP,
          "Cannot read readout area"))).flatten

  private def regularSequenceConfig[F[_]: MonadError[?[_], Throwable]](config: CleanConfig): F[GpiConfig] =
    EitherT(ApplicativeError[F, Throwable].catchNonFatal(
      (for {
        adc      <- config.extractInstAs[Adc](ADC_PROP)
        exp      <- config.extractObsAs[JDouble](EXPOSURE_TIME_PROP)
                      .map(x => Duration(x, SECONDS))
        coa      <- config.extractObsAs[JInt](COADDS_PROP)
                      .map(_.toInt)
        readMode <- gpiReadMode(config)
        area     <- gpiReadoutArea(config)
        obsMode  <- gpiMode(config)
        pol      <- config.extractInstAs[Disperser](DISPERSER_PROP)
        polA     <- config.extractInstAs[JDouble](HALF_WAVE_PLATE_ANGLE_VALUE_PROP)
                      .map(_.toDouble)
        shutters <- gpiShutters(config)
        asu      <- gpiASU(config)
        pc       <- config.extractInstAs[PupilCamera](PUPUL_CAMERA_PROP)
        ao       <- gpiAoFlags(config)
      } yield RegularGpiConfig(adc, exp, coa, readMode, area, obsMode, pol, polA, shutters, asu, pc, ao))
        .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
    )).widenRethrowT.widen[GpiConfig]

  private def alignAndCalibConfig[F[_]: Applicative]: F[GpiConfig] =
    AlignAndCalibConfig.pure[F].widen[GpiConfig]

  def fromSequenceConfig[F[_]: MonadError[?[_], Throwable]](config: CleanConfig): F[GpiConfig] =
    ApplicativeError[F, Throwable]
      .catchNonFatal(isAlignAndCalib(config))
      .ifM(alignAndCalibConfig[F], regularSequenceConfig[F](config))

}
