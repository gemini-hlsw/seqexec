// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.data.Reader
import cats.effect.Sync
import cats.implicits._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.nifs.InstNIFS._
import edu.gemini.spModel.gemini.nifs.InstEngNifs._
import edu.gemini.spModel.obscomp.InstConstants.DARK_OBSERVE_TYPE
import edu.gemini.spModel.obscomp.InstConstants.ARC_OBSERVE_TYPE
import edu.gemini.spModel.obscomp.InstConstants.FLAT_OBSERVE_TYPE
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
import seqexec.server.InstrumentSystem.UnpausableControl
import seqexec.server.InstrumentSystem.AbortObserveCmd
import seqexec.server.InstrumentSystem.StopObserveCmd
import seqexec.server.nifs.NifsController._
import seqexec.server.tcs.FOCAL_PLANE_SCALE
import squants.space.Arcseconds
import squants.{Length, Time}
import squants.time.TimeConversions._

final case class Nifs[F[_]: Sync](
  controller: NifsController[F],
  dhsClient:  DhsClient[F])
    extends DhsInstrument[F]
    with InstrumentSystem[F] {

  import Nifs._

  override def sfName(config: Config): LightSinkName = LightSinkName.Nifs

  override val contributorName: String = "NIFS"

  override val observeControl: InstrumentSystem.ObserveControl[F] =
    UnpausableControl(StopObserveCmd(SeqActionF.embedF(controller.stopObserve)),
                    AbortObserveCmd(SeqActionF.embedF(controller.abortObserve)))

  override def observe(
    config: Config
  ): SeqObserveF[F, ImageFileId, ObserveCommandResult] =
    Reader { fileId =>
      SeqActionF
        .either(getDCConfig(config).asTrySeq)
        .flatMap(x => SeqActionF.embedF(controller.observe(fileId, x)))
    }

  override def calcObserveTime(config: Config): F[Time] =
    getDCConfig(config)
      .map(controller.calcTotalExposureTime)
      .getOrElse(Sync[F].delay(60.seconds))

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
  override def configure(config: Config): SeqActionF[F, ConfigResult[F]] =
    SeqActionF
      .either(fromSequenceConfig(config))
      .flatMap(x => SeqActionF.embedF(controller.applyConfig(x)))
      .as(ConfigResult(this))

  override def notifyObserveStart: SeqActionF[F, Unit] = SeqActionF.void

  override def notifyObserveEnd: SeqActionF[F, Unit] =
    SeqActionF.embedF(controller.endObserve)
}

object Nifs {

  val name: String = INSTRUMENT_NAME_PROP

  private def centralWavelength(
    config: Config
  ): Either[ExtractFailure, CentralWavelength] =
    config
      .extractInstAs[JDouble](CENTRAL_WAVELENGTH_PROP)
      .map(_.doubleValue)
      .map(tag[CentralWavelengthD][Double])

  private def maskOffset(config: Config): Either[ExtractFailure, MaskOffset] =
    config
      .extractInstAs[JDouble](MASK_OFFSET_PROP)
      .map(_.doubleValue)
      .map(tag[MaskOffsetD][Double])

  private def windowCoverFromObserveType(observeType: String): WindowCover =
    observeType match {
      case DARK_OBSERVE_TYPE => WindowCover.Closed
      case _                 => WindowCover.Opened
    }

  private def otherCCConfig(config: Config): Either[ExtractFailure, CCConfig] =
    for {
      filter    <- config.extractInstAs[Filter](FILTER_PROP)
      mask      <- config.extractInstAs[Mask](MASK_PROP)
      disperser <- config.extractInstAs[Disperser](DISPERSER_PROP)
      imMirror  <- config.extractInstAs[ImagingMirror](IMAGING_MIRROR_PROP)
      cw        <- centralWavelength(config)
      mo        <- maskOffset(config)
      wc        <- extractObsType(config).map(windowCoverFromObserveType)
    } yield StdCCConfig(filter, mask, disperser, imMirror, cw, mo, wc)

  private def getCCConfig(config: Config): Either[ExtractFailure, CCConfig] =
    extractObsType(config).flatMap {
      case DARK_OBSERVE_TYPE => DarkCCConfig.asRight
      case _                 => otherCCConfig(config)
    }

  private def extractExposureTime(
    config: Config
  ): Either[ExtractFailure, Time] =
    config
      .extractObsAs[JDouble](EXPOSURE_TIME_PROP)
      .map(_.toDouble.seconds)

  private def extractCoadds(config: Config): Either[ExtractFailure, Coadds] =
    config
      .extractObsAs[JInt](COADDS_PROP)
      .map(_.toInt)
      .map(tag[CoaddsI][Int])

  private def extractPeriod(
    config: Config
  ): Either[ExtractFailure, Option[Period]] =
    config.extractInstInt[PeriodI](PERIOD_PROP)

  private def extractNrResets(
    config: Config
  ): Either[ExtractFailure, Option[NumberOfResets]] =
    config.extractInstInt[NumberOfResetsI](NUMBER_OF_RESETS_PROP)

  private def extractNrPeriods(
    config: Config
  ): Either[ExtractFailure, Option[NumberOfPeriods]] =
    config.extractInstInt[NumberOfPeriodsI](NUMBER_OF_PERIODS_PROP)

  private def extractObsReadMode(
    config: Config
  ): Either[ExtractFailure, ReadMode] =
    config.extractInstAs[ReadMode](READMODE_PROP)

  private def extractEngReadMode(
    config: Config
  ): Either[ExtractFailure, Option[EngReadMode]] =
    config
      .extractInstAs[EngReadMode](ENGINEERING_READMODE_PROP)
      .map(_.some)
      .recover {
        case KeyNotFound(_) => none
      }

  private def extractReadMode(
    config: Config
  ): Either[ExtractFailure, Either[EngReadMode, ReadMode]] =
    for {
      eng <- extractEngReadMode(config)
      obs <- extractObsReadMode(config)
    } yield eng.map(_.asLeft).getOrElse(obs.asRight)

  private def extractNrSamples(
    config: Config
  ): Either[ExtractFailure, Option[NumberOfSamples]] =
    config.extractInstInt[NumberOfSamplesI](NUMBER_OF_SAMPLES_PROP)

  private def extractObsType(
    config: Config
  ): Either[ExtractFailure, String] =
    config.extractAs[String](OBSERVE_KEY / OBSERVE_TYPE_PROP)

  private def getDCConfig(config: Config): Either[ExtractFailure, DCConfig] =
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

  def fromSequenceConfig(config: Config): TrySeq[NifsConfig] =
    for {
      cc <- getCCConfig(config).asTrySeq
      dc <- getDCConfig(config).asTrySeq
    } yield NifsConfig(cc, dc)

}
