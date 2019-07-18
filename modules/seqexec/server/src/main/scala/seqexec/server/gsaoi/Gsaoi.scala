// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import cats.data.Reader
import cats.effect.Sync
import cats.effect.LiftIO
import cats.implicits._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gsaoi.Gsaoi._
import edu.gemini.spModel.obscomp.InstConstants.DARK_OBSERVE_TYPE
import edu.gemini.spModel.obscomp.InstConstants.OBSERVE_TYPE_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.OBSERVE_KEY
import gem.enum.LightSinkName
import java.lang.{Double => JDouble}
import java.lang.{Integer => JInt}
import shapeless.tag
import seqexec.server.ConfigUtilOps._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Instrument
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.ConfigUtilOps.ExtractFailure
import seqexec.server.ConfigResult
import seqexec.server.InstrumentSystem
import seqexec.server.Progress
import seqexec.server.SeqActionF
import seqexec.server.SeqObserveF
import seqexec.server.TrySeq
import seqexec.server.keywords.DhsClient
import seqexec.server.keywords.DhsInstrument
import seqexec.server.keywords.KeywordsClient
import seqexec.server.InstrumentSystem._
import seqexec.server.gsaoi.GsaoiController._
import seqexec.server.tcs.FOCAL_PLANE_SCALE
import squants.space.Arcseconds
import squants.{Length, Time}
import squants.time.TimeConversions._

final case class Gsaoi[F[_]: LiftIO: Sync](
  controller: GsaoiController[F],
  dhsClient:  DhsClient[F])
    extends DhsInstrument[F]
    with InstrumentSystem[F] {

  import Gsaoi._

  override def sfName(config: Config): LightSinkName = LightSinkName.Gsaoi

  override val contributorName: String = "GSAOI"

  override val observeControl: InstrumentSystem.ObserveControl[F] =
    UnpausableControl[F](StopObserveCmd[F](SeqActionF.embedF(controller.stopObserve)),
                 AbortObserveCmd[F](SeqActionF.embedF(controller.abortObserve)))

  override def observe(
    config: Config
  ): SeqObserveF[F, ImageFileId, ObserveCommandResult] =
    Reader { fileId =>
      SeqActionF
        .either(readDCConfig(config).asTrySeq)
        .flatMap(x => SeqActionF.embedF(controller.observe(fileId, x)))
    }

  override def calcObserveTime(config: Config): F[Time] =
    readDCConfig(config)
      .map(controller.calcTotalExposureTime)
      .getOrElse(Sync[F].delay(60.seconds))

  override def keywordsClient: KeywordsClient[F] = this

  override def observeProgress(
    total:   Time,
    elapsed: InstrumentSystem.ElapsedTime
  ): fs2.Stream[F, Progress] =
    controller.observeProgress(total)

  override val oiOffsetGuideThreshold: Option[Length] = (Arcseconds(0.01)/FOCAL_PLANE_SCALE).some

  override val dhsInstrumentName: String = "GSAOI"

  override val resource: Instrument = Instrument.Gsaoi

  /**
    * Called to configure a system
    */
  override def configure(config: Config): SeqActionF[F, ConfigResult[F]] =
    SeqActionF
      .either(fromSequenceConfig(config))
      .flatMap(x => SeqActionF.embedF(controller.applyConfig(x)))
      .as(ConfigResult(this))

  override def notifyObserveStart: SeqActionF[F, Unit] = SeqActionF.void

  override def notifyObserveEnd: SeqActionF[F, Unit] =
    SeqActionF.embedF(controller.endObserve)
}

object Gsaoi {

  val name: String = INSTRUMENT_NAME_PROP

  private def extractObsType(
    config: Config
  ): Either[ExtractFailure, String] =
    config.extractAs[String](OBSERVE_KEY / OBSERVE_TYPE_PROP)

  private def readCCConfig(config: Config): Either[ExtractFailure, CCConfig] =
    for {
      obsType      <- extractObsType(config)
      filter       <- config.extractInstAs[Filter](FILTER_PROP)
      odgwSize     <- config.extractInstAs[OdgwSize](ODGW_SIZE_PROP)
      utilityWheel <- config.extractInstAs[UtilityWheel](UTILITY_WHEEL_PROP)
    } yield
      obsType match {
        case DARK_OBSERVE_TYPE => CCConfig(Filter.BLOCKED, odgwSize, utilityWheel, WindowCover.Closed)
        case _ => CCConfig(filter, odgwSize, utilityWheel, WindowCover.Opened)
      }

  private def extractExposureTime(
    config: Config
  ): Either[ExtractFailure, Time] =
    config
      .extractObsAs[JDouble](EXPOSURE_TIME_PROP)
      .map(_.toDouble.seconds)

  private def extractCoadds(config: Config): Either[ExtractFailure, Coadds] =
    config
      .extractInstAs[JInt](COADDS_PROP)
      .map(_.toInt)
      .map(tag[CoaddsI][Int])

  private def extractReadMode(
    config: Config
  ): Either[ExtractFailure, ReadMode] =
    config.extractInstAs[ReadMode](READ_MODE_PROP)

  private def extractRoi(
    config: Config
  ): Either[ExtractFailure, Roi] =
    config.extractInstAs[Roi](ROI_PROP)

  private def extractNrOfFowSamples(
    config: Config
  ): Either[ExtractFailure, NumberOfFowSamples] =
    extractReadMode(config).map {
      case ReadMode.BRIGHT      => 1
      case ReadMode.FAINT       => 4
      case ReadMode.VERY_FAINT  => 8
    }.map(tag[NumberOfFowSamplesI][Int])

  private def readDCConfig(config: Config): Either[ExtractFailure, DCConfig] =
    for {
      readMode   <- extractReadMode(config)
      roi        <- extractRoi(config)
      coadds     <- extractCoadds(config)
      expTime    <- extractExposureTime(config)
      fowSamples <- extractNrOfFowSamples(config)
    } yield DCConfig(readMode, roi, coadds, expTime, fowSamples)

  def fromSequenceConfig(config: Config): TrySeq[GsaoiConfig] =
    for {
      cc <- readCCConfig(config).asTrySeq
      dc <- readDCConfig(config).asTrySeq
    } yield GsaoiConfig(cc, dc)

}
