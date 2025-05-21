// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import java.lang.{ Double => JDouble }
import java.lang.{ Integer => JInt }
import cats.data.EitherT
import cats.data.Kleisli
import cats.effect.{ Async, Sync }
import cats.syntax.all._
import edu.gemini.spModel.gemini.gsaoi.Gsaoi._
import edu.gemini.spModel.obscomp.InstConstants.DARK_OBSERVE_TYPE
import edu.gemini.spModel.obscomp.InstConstants.OBSERVE_TYPE_PROP
import org.typelevel.log4cats.Logger
import lucuma.core.enums.LightSinkName
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Instrument
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.CleanConfig
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigResult
import seqexec.server.ConfigUtilOps
import seqexec.server.ConfigUtilOps.ExtractFailure
import seqexec.server.ConfigUtilOps._
import seqexec.server.InstrumentActions
import seqexec.server.InstrumentSpecifics
import seqexec.server.InstrumentSystem
import seqexec.server.InstrumentSystem._
import seqexec.server.Progress
import seqexec.server.SeqexecFailure
import seqexec.server.gsaoi.GsaoiController._
import seqexec.server.keywords.{ DhsClient, DhsClientProvider, DhsInstrument, KeywordsClient }
import seqexec.server.tcs.FOCAL_PLANE_SCALE
import shapeless.tag
import squants.Length
import squants.Time
import squants.space.Arcseconds
import squants.time.TimeConversions._
import cats.Applicative

final case class Gsaoi[F[_]: Logger: Async](
  controller:        GsaoiController[F],
  dhsClientProvider: DhsClientProvider[F]
) extends DhsInstrument[F]
    with InstrumentSystem[F] {

  import Gsaoi._

  override val contributorName: String = "GSAOI"

  override def observeControl(config: CleanConfig): InstrumentSystem.ObserveControl[F] =
    UnpausableControl(
      StopObserveCmd(_ => controller.stopObserve),
      AbortObserveCmd(controller.abortObserve)
    )

  override def observe(
    config: CleanConfig
  ): Kleisli[F, ImageFileId, ObserveCommandResult] =
    Kleisli { fileId =>
      EitherT
        .fromEither[F] {
          readDCConfig(config)
            .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
        }
        .widenRethrowT
        .flatMap(x => controller.observe(fileId, x))
    }

  override val sequenceComplete: F[Unit] = Applicative[F].unit

  override def calcObserveTime(config: CleanConfig): F[Time] =
    readDCConfig(config)
      .map(controller.calcTotalExposureTime)
      .getOrElse(Sync[F].delay(60.seconds))

  override def keywordsClient: KeywordsClient[F] = this

  override def observeProgress(
    total:   Time,
    elapsed: InstrumentSystem.ElapsedTime
  ): fs2.Stream[F, Progress] =
    controller.observeProgress(total)

  override val dhsInstrumentName: String = "GSAOI"

  override val dhsClient: DhsClient[F] = dhsClientProvider.dhsClient(dhsInstrumentName)

  override val resource: Instrument = Instrument.Gsaoi

  /**
   * Called to configure a system
   */
  override def configure(config: CleanConfig): F[ConfigResult[F]] =
    EitherT
      .fromEither[F](fromSequenceConfig(config))
      .widenRethrowT
      .flatMap(controller.applyConfig)
      .as(ConfigResult(this))

  override def notifyObserveStart: F[Unit] = Sync[F].unit

  override def notifyObserveEnd: F[Unit] =
    controller.endObserve

  override def instrumentActions(config: CleanConfig): InstrumentActions[F] =
    InstrumentActions.defaultInstrumentActions[F]
}

object Gsaoi {

  val name: String = INSTRUMENT_NAME_PROP

  private def extractObsType(
    config: CleanConfig
  ): Either[ExtractFailure, String] =
    config.extractObsAs[String](OBSERVE_TYPE_PROP)

  private def readCCConfig(config: CleanConfig): Either[ExtractFailure, CCConfig] =
    for {
      obsType      <- extractObsType(config)
      filter       <- config.extractInstAs[Filter](FILTER_PROP)
      odgwSize     <- config.extractInstAs[OdgwSize](ODGW_SIZE_PROP)
      utilityWheel <- config.extractInstAs[UtilityWheel](UTILITY_WHEEL_PROP)
    } yield obsType match {
      case DARK_OBSERVE_TYPE => CCConfig(Filter.BLOCKED, odgwSize, utilityWheel, WindowCover.Closed)
      case _                 => CCConfig(filter, odgwSize, utilityWheel, WindowCover.Opened)
    }

  private def extractExposureTime(
    config: CleanConfig
  ): Either[ExtractFailure, Time] =
    config
      .extractObsAs[JDouble](EXPOSURE_TIME_PROP)
      .map(_.toDouble.seconds)

  private def extractCoadds(config: CleanConfig): Either[ExtractFailure, Coadds] =
    config
      .extractInstAs[JInt](COADDS_PROP)
      .map(_.toInt)
      .map(tag[CoaddsI][Int])

  private def extractReadMode(
    config: CleanConfig
  ): Either[ExtractFailure, ReadMode] =
    config.extractInstAs[ReadMode](READ_MODE_PROP)

  private def extractRoi(
    config: CleanConfig
  ): Either[ExtractFailure, Roi] =
    config.extractInstAs[Roi](ROI_PROP)

  private def extractNrOfFowSamples(
    config: CleanConfig
  ): Either[ExtractFailure, NumberOfFowSamples] =
    extractReadMode(config)
      .map {
        case ReadMode.BRIGHT     => 1
        case ReadMode.FAINT      => 4
        case ReadMode.VERY_FAINT => 8
      }
      .map(tag[NumberOfFowSamplesI][Int])

  private def readDCConfig(config: CleanConfig): Either[ExtractFailure, DCConfig] =
    for {
      readMode   <- extractReadMode(config)
      roi        <- extractRoi(config)
      coadds     <- extractCoadds(config)
      expTime    <- extractExposureTime(config)
      fowSamples <- extractNrOfFowSamples(config)
    } yield DCConfig(readMode, roi, coadds, expTime, fowSamples)

  def fromSequenceConfig(config: CleanConfig): Either[SeqexecFailure, GsaoiConfig] =
    for {
      cc <- readCCConfig(config).adaptExtractFailure
      dc <- readDCConfig(config).adaptExtractFailure
    } yield GsaoiConfig(cc, dc)

  object specifics extends InstrumentSpecifics {
    override val instrument: Instrument = Instrument.Gsaoi

    override def sfName(config: CleanConfig): LightSinkName = LightSinkName.Gsaoi

    override val oiOffsetGuideThreshold: Option[Length] =
      (Arcseconds(0.01) / FOCAL_PLANE_SCALE).some

  }

}
