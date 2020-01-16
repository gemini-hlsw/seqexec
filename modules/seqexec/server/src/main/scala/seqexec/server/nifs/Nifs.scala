// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.data.Kleisli
import cats.data.EitherT
import cats.effect.{Concurrent, Timer, Sync}
import cats.implicits._
import edu.gemini.spModel.gemini.nifs.InstNIFS._
import edu.gemini.spModel.gemini.nifs.InstEngNifs._
import edu.gemini.spModel.obscomp.InstConstants.DARK_OBSERVE_TYPE
import edu.gemini.spModel.obscomp.InstConstants.ARC_OBSERVE_TYPE
import edu.gemini.spModel.obscomp.InstConstants.FLAT_OBSERVE_TYPE
import edu.gemini.spModel.obscomp.InstConstants.OBSERVE_TYPE_PROP
import gem.enum.LightSinkName
import io.chrisdavenport.log4cats.Logger
import java.lang.{Double => JDouble}
import java.lang.{Integer => JInt}

import shapeless.tag
import seqexec.server.ConfigUtilOps._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Instrument
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.ConfigUtilOps.ExtractFailure
import seqexec.server.{CleanConfig, ConfigResult, InstrumentActions, InstrumentSystem, Progress, TrySeq}
import seqexec.server.CleanConfig.extractItem
import seqexec.server.keywords.DhsClient
import seqexec.server.keywords.DhsInstrument
import seqexec.server.keywords.KeywordsClient
import seqexec.server.InstrumentSystem.UnpausableControl
import seqexec.server.InstrumentSystem.AbortObserveCmd
import seqexec.server.InstrumentSystem.StopObserveCmd
import seqexec.server.nifs.NifsController._
import seqexec.server.tcs.FOCAL_PLANE_SCALE
import squants.space.Arcseconds
import squants.{Length, Time}
import squants.time.TimeConversions._

final case class Nifs[F[_]: Logger: Concurrent: Timer](
  controller: NifsController[F],
  dhsClient:  DhsClient[F])
    extends DhsInstrument[F]
    with InstrumentSystem[F] {

  import Nifs._

  override def sfName(config: CleanConfig): LightSinkName = LightSinkName.Nifs

  override val contributorName: String = "NIFS"

  override def observeControl(config: CleanConfig): InstrumentSystem.ObserveControl[F] =
    UnpausableControl(StopObserveCmd(_ => controller.stopObserve),
      AbortObserveCmd(controller.abortObserve))

  override def observe(
    config: CleanConfig
  ): Kleisli[F, ImageFileId, ObserveCommandResult] =
    Kleisli { fileId =>
      EitherT.fromEither[F](getDCConfig(config).asTrySeq)
        .widenRethrowT
        .flatMap(controller.observe(fileId, _))
    }

  override def calcObserveTime(config: CleanConfig): F[Time] =
    getDCConfig(config)
      .map(controller.calcTotalExposureTime)
      .getOrElse(60.seconds).pure[F]

  override def keywordsClient: KeywordsClient[F] = this

  override def observeProgress(
    total:   Time,
    elapsed: InstrumentSystem.ElapsedTime
  ): fs2.Stream[F, Progress] =
    controller.observeProgress(total)

  override val oiOffsetGuideThreshold: Option[Length] = (Arcseconds(0.01)/FOCAL_PLANE_SCALE).some

  override val dhsInstrumentName: String = "NIFS"

  override val resource: Instrument = Instrument.Nifs

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

object Nifs {

  val name: String = INSTRUMENT_NAME_PROP

  private def centralWavelength(
    config: CleanConfig
  ): Either[ExtractFailure, CentralWavelength] =
    config
      .extractInstAs[JDouble](CENTRAL_WAVELENGTH_PROP)
      .map(_.doubleValue)
      .map(tag[CentralWavelengthD][Double])

  private def maskOffset(config: CleanConfig): Either[ExtractFailure, MaskOffset] =
    config
      .extractInstAs[JDouble](MASK_OFFSET_PROP)
      .map(_.doubleValue)
      .map(tag[MaskOffsetD][Double])

  private def windowCoverFromObserveType(observeType: String): WindowCover =
    observeType match {
      case DARK_OBSERVE_TYPE => WindowCover.Closed
      case _                 => WindowCover.Opened
    }

  private def otherCCConfig(config: CleanConfig): Either[ExtractFailure, CCConfig] =
    for {
      filter    <- config.extractInstAs[Filter](FILTER_PROP)
      mask      <- config.extractInstAs[Mask](MASK_PROP)
      disperser <- config.extractInstAs[Disperser](DISPERSER_PROP)
      imMirror  <- config.extractInstAs[ImagingMirror](IMAGING_MIRROR_PROP)
      cw        <- centralWavelength(config)
      mo        <- maskOffset(config)
      wc        <- extractObsType(config).map(windowCoverFromObserveType)
    } yield StdCCConfig(filter, mask, disperser, imMirror, cw, mo, wc)

  private def getCCConfig(config: CleanConfig): Either[ExtractFailure, CCConfig] =
    extractObsType(config).flatMap {
      case DARK_OBSERVE_TYPE => DarkCCConfig.asRight
      case _                 => otherCCConfig(config)
    }

  private def extractExposureTime(
    config: CleanConfig
  ): Either[ExtractFailure, Time] =
    config
      .extractObsAs[JDouble](EXPOSURE_TIME_PROP)
      .map(_.toDouble.seconds)

  private def extractCoadds(config: CleanConfig): Either[ExtractFailure, Coadds] =
    config
      .extractObsAs[JInt](COADDS_PROP)
      .map(_.toInt)
      .map(tag[CoaddsI][Int])

  private def extractPeriod(
    config: CleanConfig
  ): Either[ExtractFailure, Option[Period]] =
    config.extractInstInt[PeriodI](PERIOD_PROP)

  private def extractNrResets(
    config: CleanConfig
  ): Either[ExtractFailure, Option[NumberOfResets]] =
    config.extractInstInt[NumberOfResetsI](NUMBER_OF_RESETS_PROP)

  private def extractNrPeriods(
    config: CleanConfig
  ): Either[ExtractFailure, Option[NumberOfPeriods]] =
    config.extractInstInt[NumberOfPeriodsI](NUMBER_OF_PERIODS_PROP)

  private def extractObsReadMode(
    config: CleanConfig
  ): Either[ExtractFailure, ReadMode] =
    config.extractInstAs[ReadMode](READMODE_PROP)

  private def extractEngReadMode(
    config: CleanConfig
  ): Either[ExtractFailure, Option[EngReadMode]] =
    config
      .extractInstAs[EngReadMode](ENGINEERING_READMODE_PROP)
      .map(_.some)
      .recover {
        case KeyNotFound(_) => none
      }

  private def extractReadMode(
    config: CleanConfig
  ): Either[ExtractFailure, Either[EngReadMode, ReadMode]] =
    for {
      eng <- extractEngReadMode(config)
      obs <- extractObsReadMode(config)
    } yield eng.map(_.asLeft).getOrElse(obs.asRight)

  private def extractNrSamples(
    config: CleanConfig
  ): Either[ExtractFailure, Option[NumberOfSamples]] =
    config.extractInstInt[NumberOfSamplesI](NUMBER_OF_SAMPLES_PROP)

  private def extractObsType(
    config: CleanConfig
  ): Either[ExtractFailure, String] =
    config.extractObsAs[String](OBSERVE_TYPE_PROP)

  private def getDCConfig(config: CleanConfig): Either[ExtractFailure, DCConfig] =
    for {
      coadds    <- extractCoadds(config)
      period    <- extractPeriod(config)
      expTime   <- extractExposureTime(config)
      nrResets  <- extractNrResets(config)
      nrPeriods <- extractNrPeriods(config)
      readMode  <- extractReadMode(config)
      samples   <- extractNrSamples(config)
      obsType   <- extractObsType(config)
    } yield
      obsType match {
        case ARC_OBSERVE_TYPE | FLAT_OBSERVE_TYPE =>
          ArcFlatDCConfig(coadds, period, expTime, nrResets, nrPeriods, samples)
        case _ =>
          StdDCConfig(coadds, period, expTime, nrResets, nrPeriods, samples, readMode)
      }

  def fromSequenceConfig(config: CleanConfig): TrySeq[NifsConfig] =
    for {
      cc <- getCCConfig(config).asTrySeq
      dc <- getDCConfig(config).asTrySeq
    } yield NifsConfig(cc, dc)

}
