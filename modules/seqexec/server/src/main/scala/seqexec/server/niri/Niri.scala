// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import cats.data.EitherT
import cats.data.Kleisli
import cats.effect.{Concurrent, Sync, Timer}
import cats.implicits._
import edu.gemini.spModel.gemini.niri.InstNIRI._
import edu.gemini.spModel.gemini.niri.Niri.{Camera, WellDepth, ReadMode => OCSReadMode}
import edu.gemini.spModel.obscomp.InstConstants.{BIAS_OBSERVE_TYPE, DARK_OBSERVE_TYPE, OBSERVE_TYPE_PROP}
import edu.gemini.seqexec.server.niri.ReadMode
import gem.enum.LightSinkName
import io.chrisdavenport.log4cats.Logger
import seqexec.server.ConfigUtilOps._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.model.enum.Instrument
import seqexec.server.ConfigUtilOps.ExtractFailure
import seqexec.server.{CleanConfig, ConfigResult, ConfigUtilOps, InstrumentActions, InstrumentSystem, Progress, SeqexecFailure, TrySeq}
import seqexec.server.CleanConfig.extractItem
import seqexec.server.keywords.{DhsClient, DhsInstrument, KeywordsClient}
import seqexec.server.tcs.FOCAL_PLANE_SCALE
import java.lang.{Double => JDouble, Integer => JInt}

import seqexec.server.InstrumentSystem.UnpausableControl
import seqexec.server.InstrumentSystem.AbortObserveCmd
import seqexec.server.InstrumentSystem.StopObserveCmd
import seqexec.server.niri.NiriController._
import squants.space.Arcseconds
import squants.{Length, Time}
import squants.time.TimeConversions._

final case class Niri[F[_]: Timer: Logger: Concurrent](controller: NiriController[F], dhsClient: DhsClient[F])
  extends DhsInstrument[F] with InstrumentSystem[F] {

  import Niri._

  override def sfName(config: CleanConfig): LightSinkName = getCameraConfig(config).map{
    case Camera.F6     => LightSinkName.Niri_f6
    case Camera.F14    => LightSinkName.Niri_f14
    case Camera.F32 |
         Camera.F32_PV => LightSinkName.Niri_f32
  }.getOrElse(LightSinkName.Niri_f6)

  override val contributorName: String = "mko-dc-data-niri"
  override def observeControl(config: CleanConfig): InstrumentSystem.ObserveControl[F] =
    UnpausableControl(
      StopObserveCmd(_ => controller.stopObserve),
      AbortObserveCmd(controller.abortObserve)
    )

  override def observe(config: CleanConfig): Kleisli[F, ImageFileId, ObserveCommandResult] =
    Kleisli { fileId =>
      EitherT.fromEither[F](getDCConfig(config))
        .widenRethrowT
        .flatMap(controller.observe(fileId, _))
    }

  override def calcObserveTime(config: CleanConfig): F[Time] =
    getDCConfig(config)
      .map(controller.calcTotalExposureTime)
      .getOrElse(60.seconds.pure[F])

  override def observeProgress(total: Time, elapsed: InstrumentSystem.ElapsedTime)
  : fs2.Stream[F, Progress] = controller.observeProgress(total)

  override val oiOffsetGuideThreshold: Option[Length] = (Arcseconds(0.01)/FOCAL_PLANE_SCALE).some

  override val dhsInstrumentName: String = "NIRI"

  override val keywordsClient: KeywordsClient[F] = this

  override val resource: Instrument = Instrument.Niri

  /**
    * Called to configure a system
    */
  override def configure(config: CleanConfig): F[ConfigResult[F]] =
    EitherT.fromEither[F](fromSequenceConfig(config))
      .widenRethrowT
      .flatMap(controller.applyConfig)
      .as(ConfigResult(this))

  override def notifyObserveStart: F[Unit] = Sync[F].unit

  override def notifyObserveEnd: F[Unit] =
    controller.endObserve

  override def instrumentActions(config: CleanConfig): InstrumentActions[F] =
    InstrumentActions.defaultInstrumentActions[F]
}

object Niri {
  val name: String = INSTRUMENT_NAME_PROP

  def extractExposureTime(config: CleanConfig): Either[ExtractFailure, Time] =
    config.extractObsAs[JDouble](EXPOSURE_TIME_PROP).map(_.toDouble.seconds)

  def extractCoadds(config: CleanConfig): Either[ExtractFailure, Int] =
    config.extractObsAs[JInt](COADDS_PROP).map(_.toInt)

  def calcReadMode(readMode: OCSReadMode, wellDepth: WellDepth)
  : Either[ConfigUtilOps.ExtractFailure, NiriController.ReadMode] = {
    import OCSReadMode._
    import WellDepth._
    (readMode, wellDepth) match {
      case (IMAG_SPEC_NB, SHALLOW)   => ReadMode.LowRN.asRight
      case (IMAG_1TO25, SHALLOW)     => ReadMode.MedRN.asRight
      case (IMAG_SPEC_3TO5, SHALLOW) => ReadMode.HighRN.asRight
      case (IMAG_1TO25, DEEP)        => ReadMode.MedRNDeep.asRight
      case (IMAG_SPEC_3TO5, DEEP   ) => ReadMode.ThermalIR.asRight
      case _                         => ContentError(s"Combination not supported: readMode = " +
        s"${readMode.displayValue}, wellDepth = ${wellDepth.displayValue}").asLeft
    }
  }

  def getCameraConfig(config: CleanConfig): Either[ExtractFailure, Camera] =
    config.extractInstAs[Camera](CAMERA_PROP)

  def getCCCommonConfig(config: CleanConfig): TrySeq[Common] = (for {
    cam <- getCameraConfig(config)
    bms <- config.extractInstAs[BeamSplitter](BEAM_SPLITTER_PROP)
    foc <- config.extractInstAs[Focus](FOCUS_PROP)
    dsp <- config.extractInstAs[Disperser](DISPERSER_PROP)
    msk <- config.extractInstAs[Mask](MASK_PROP)
  } yield Common(cam, bms, foc, dsp, msk))
    .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  def getCCIlluminatedConfig(config: CleanConfig): TrySeq[Illuminated] = {
    val filter = (for {
      f  <- config.extractInstAs[Filter](FILTER_PROP)
      fl <- if(f.isObsolete) ContentError(s"Obsolete filter ${f.displayValue}").asLeft
      else f.asRight
    } yield fl)
      .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

    (filter, getCCCommonConfig(config)).mapN(Illuminated)
  }

  def getCCDarkConfig(config: CleanConfig): TrySeq[Dark] = getCCCommonConfig(config).map(Dark)

  def getCCConfig(config: CleanConfig): TrySeq[CCConfig] =
    config.extractObsAs[String](OBSERVE_TYPE_PROP)
      .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
      .flatMap{
        case DARK_OBSERVE_TYPE => getCCDarkConfig(config)
        case BIAS_OBSERVE_TYPE => SeqexecFailure.Unexpected("Bias not supported for NIRI").asLeft
        case _                 => getCCIlluminatedConfig(config)
      }

  def getDCConfig(config: CleanConfig): TrySeq[DCConfig] = (for {
      expTime    <- extractExposureTime(config)
      coadds     <- extractCoadds(config)
      rm         <- config.extractInstAs[OCSReadMode](READ_MODE_PROP)
      wellDepth  <- config.extractInstAs[WellDepth](WELL_DEPTH_PROP)
      readMode   <- calcReadMode(rm, wellDepth)
      builtInROI <- config.extractInstAs[BuiltInROI](BUILTIN_ROI_PROP)
    } yield DCConfig(expTime, coadds, readMode, builtInROI))
      .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  def fromSequenceConfig(config: CleanConfig): TrySeq[NiriConfig] = for {
    cc <- getCCConfig(config)
    dc <- getDCConfig(config)
  } yield NiriConfig(cc, dc)

}
