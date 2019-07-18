// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import cats.data.Reader
import cats.effect.Sync
import cats.effect.Timer
import cats.implicits._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.seqcomp.SeqConfigNames.{INSTRUMENT_KEY, OBSERVE_KEY}
import edu.gemini.spModel.gemini.niri.InstNIRI._
import edu.gemini.spModel.gemini.niri.Niri.{Camera, WellDepth, ReadMode => OCSReadMode}
import edu.gemini.spModel.obscomp.InstConstants.{BIAS_OBSERVE_TYPE, DARK_OBSERVE_TYPE, OBSERVE_TYPE_PROP}
import edu.gemini.seqexec.server.niri.ReadMode
import gem.enum.LightSinkName
import seqexec.server.ConfigUtilOps._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.model.enum.Instrument
import seqexec.server.ConfigUtilOps.ExtractFailure
import seqexec.server.{ConfigResult, ConfigUtilOps, InstrumentSystem, Progress, SeqActionF, SeqObserveF, SeqexecFailure, TrySeq}
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

final case class Niri[F[_]: Sync: Timer](controller: NiriController[F], dhsClient: DhsClient[F])
  extends DhsInstrument[F] with InstrumentSystem[F] {

  import Niri._

  override def sfName(config: Config): LightSinkName = getCameraConfig(config).map{
    case Camera.F6     => LightSinkName.Niri_f6
    case Camera.F14    => LightSinkName.Niri_f14
    case Camera.F32 |
         Camera.F32_PV => LightSinkName.Niri_f32
  }.getOrElse(LightSinkName.Niri_f6)

  override val contributorName: String = "mko-dc-data-niri"
  override val observeControl: InstrumentSystem.ObserveControl[F] =
    UnpausableControl(StopObserveCmd(SeqActionF.embedF(controller.stopObserve)),
                    AbortObserveCmd(SeqActionF.embedF(controller.abortObserve)))

  override def observe(config: Config): SeqObserveF[F, ImageFileId, ObserveCommandResult] =
    Reader { fileId => SeqActionF.either(getDCConfig(config))
      .flatMap(x => SeqActionF.embedF(controller.observe(fileId, x)))
    }

  override def calcObserveTime(config: Config): F[Time] =
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
  override def configure(config: Config): SeqActionF[F, ConfigResult[F]] =
    SeqActionF.either(fromSequenceConfig(config))
      .flatMap(x => SeqActionF.embedF(controller.applyConfig(x))).as(ConfigResult(this))

  override def notifyObserveStart: SeqActionF[F, Unit] = SeqActionF.void

  override def notifyObserveEnd: SeqActionF[F, Unit] =
    SeqActionF.embedF(controller.endObserve)
}

object Niri {
  val name: String = INSTRUMENT_NAME_PROP

  def extractExposureTime(config: Config): Either[ExtractFailure, Time] =
    config.extractAs[JDouble](OBSERVE_KEY / EXPOSURE_TIME_PROP).map(_.toDouble.seconds)

  def extractCoadds(config: Config): Either[ExtractFailure, Int] =
    config.extractAs[JInt](OBSERVE_KEY / COADDS_PROP).map(_.toInt)

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

  def getCameraConfig(config: Config): Either[ExtractFailure, Camera] =
    config.extractAs[Camera](INSTRUMENT_KEY / CAMERA_PROP)

  def getCCCommonConfig(config: Config): TrySeq[Common] = (for {
    cam <- getCameraConfig(config)
    bms <- config.extractAs[BeamSplitter](INSTRUMENT_KEY / BEAM_SPLITTER_PROP)
    foc <- config.extractAs[Focus](INSTRUMENT_KEY / FOCUS_PROP)
    dsp <- config.extractAs[Disperser](INSTRUMENT_KEY / DISPERSER_PROP)
    msk <- config.extractAs[Mask](INSTRUMENT_KEY / MASK_PROP)
  } yield Common(cam, bms, foc, dsp, msk))
    .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  def getCCIlluminatedConfig(config: Config): TrySeq[Illuminated] = {
    val filter = (for {
      f  <- config.extractAs[Filter](INSTRUMENT_KEY / FILTER_PROP)
      fl <- if(f.isObsolete) ContentError(s"Obsolete filter ${f.displayValue}").asLeft
      else f.asRight
    } yield fl)
      .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

    (filter, getCCCommonConfig(config)).mapN(Illuminated(_, _))
  }


  def getCCDarkConfig(config: Config): TrySeq[Dark] = getCCCommonConfig(config).map(Dark(_))

  def getCCConfig(config: Config): TrySeq[CCConfig] =
    config.extractAs[String](OBSERVE_KEY / OBSERVE_TYPE_PROP)
      .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
      .flatMap{
        case DARK_OBSERVE_TYPE => getCCDarkConfig(config)
        case BIAS_OBSERVE_TYPE => SeqexecFailure.Unexpected("Bias not supported for NIRI").asLeft
        case _                 => getCCIlluminatedConfig(config)
      }

  def getDCConfig(config: Config): TrySeq[DCConfig] = (for {
      expTime    <- extractExposureTime(config)
      coadds     <- extractCoadds(config)
      rm         <- config.extractAs[OCSReadMode](INSTRUMENT_KEY / READ_MODE_PROP)
      wellDepth  <- config.extractAs[WellDepth](INSTRUMENT_KEY / WELL_DEPTH_PROP)
      readMode   <- calcReadMode(rm, wellDepth)
      builtInROI <- config.extractAs[BuiltInROI](INSTRUMENT_KEY / BUILTIN_ROI_PROP)
    } yield DCConfig(expTime, coadds, readMode, builtInROI))
      .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  def fromSequenceConfig(config: Config): TrySeq[NiriConfig] = for {
    cc <- getCCConfig(config)
    dc <- getDCConfig(config)
  } yield NiriConfig(cc, dc)

}
