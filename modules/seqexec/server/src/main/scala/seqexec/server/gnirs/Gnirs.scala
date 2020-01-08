// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.data.Kleisli
import cats.data.EitherT
import cats.implicits._
import cats.effect.{Concurrent, Sync, Timer}
import edu.gemini.spModel.gemini.gnirs.GNIRSConstants.{INSTRUMENT_NAME_PROP, WOLLASTON_PRISM_PROP}
import edu.gemini.spModel.gemini.gnirs.GNIRSParams._
import edu.gemini.spModel.gemini.gnirs.InstGNIRS._
import edu.gemini.spModel.obscomp.InstConstants.{BIAS_OBSERVE_TYPE, DARK_OBSERVE_TYPE, OBSERVE_TYPE_PROP}
import java.lang.{Double => JDouble, Integer => JInt}

import gem.enum.LightSinkName
import gsp.math.syntax.string._
import io.chrisdavenport.log4cats.Logger
import seqexec.model.enum.ObserveCommandResult
import seqexec.model.enum.Instrument
import seqexec.model.dhs.ImageFileId
import seqexec.server.ConfigUtilOps._
import seqexec.server.gnirs.GnirsController.{CCConfig, DCConfig, Filter1, Other, ReadMode}
import seqexec.server._
import seqexec.server.CleanConfig.extractItem
import seqexec.server.keywords.{DhsClient, DhsInstrument, KeywordsClient}
import squants.Time
import squants.space.LengthConversions._
import squants.time.TimeConversions._

final case class Gnirs[F[_]: Logger: Concurrent: Timer](controller: GnirsController[F], dhsClient: DhsClient[F]) extends DhsInstrument[F] with InstrumentSystem[F] {
  override def sfName(config: CleanConfig): LightSinkName = LightSinkName.Gnirs
  override val contributorName: String = "ngnirsdc1"
  override val dhsInstrumentName: String = "GNIRS"

  override val keywordsClient: KeywordsClient[F] = this

  import Gnirs._
  import InstrumentSystem._
  override def observeControl(config: CleanConfig): ObserveControl[F] =
    UnpausableControl(
      StopObserveCmd(_ => controller.stopObserve),
      AbortObserveCmd(controller.abortObserve)
    )

  override def observe(config: CleanConfig): Kleisli[F, ImageFileId, ObserveCommandResult] =
    Kleisli { fileId =>
      calcObserveTime(config).flatMap { x =>
        controller.observe(fileId, x)
      }
    }

  override def calcObserveTime(config: CleanConfig): F[Time] =
    getDCConfig(config)
      .map(controller.calcTotalExposureTime)
      .getOrElse(60.seconds.pure[F])

  override val resource: Instrument = Instrument.Gnirs

  override def configure(config: CleanConfig): F[ConfigResult[F]] =
    EitherT.fromEither[F](fromSequenceConfig(config))
      .widenRethrowT
      .flatMap(controller.applyConfig)
      .as(ConfigResult(this))

  override def notifyObserveEnd: F[Unit] =
    controller.endObserve

  override def notifyObserveStart: F[Unit] = Sync[F].unit

  override def observeProgress(total: Time, elapsed: ElapsedTime): fs2.Stream[F, Progress] =
    controller.observeProgress(total)

  override def instrumentActions(config: CleanConfig): InstrumentActions[F] =
    InstrumentActions.defaultInstrumentActions[F]

}

object Gnirs {

  val name: String = INSTRUMENT_NAME_PROP

  def extractExposureTime(config: CleanConfig): Either[ExtractFailure, Time] =
    config.extractObsAs[JDouble](EXPOSURE_TIME_PROP).map(_.toDouble.seconds)

  def extractCoadds(config: CleanConfig): Either[ExtractFailure, Int] =
    config.extractObsAs[JInt](COADDS_PROP).map(_.toInt)

  def fromSequenceConfig(config: CleanConfig): TrySeq[GnirsController.GnirsConfig] =
    (getCCConfig(config), getDCConfig(config)).mapN(GnirsController.GnirsConfig)

  private def getDCConfig(config: CleanConfig): TrySeq[DCConfig] = (for {
    expTime <- extractExposureTime(config)
    coadds  <- extractCoadds(config)
    readMode <- config.extractInstAs[ReadMode](READ_MODE_PROP)
    wellDepth <- config.extractInstAs[WellDepth](WELL_DEPTH_PROP)
  } yield DCConfig(expTime, coadds, readMode, wellDepth))
    .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  private def getCCConfig(config: CleanConfig): TrySeq[CCConfig] = config.extractObsAs[String](OBSERVE_TYPE_PROP)
    .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))).flatMap{
    case DARK_OBSERVE_TYPE => GnirsController.Dark.asRight
    case BIAS_OBSERVE_TYPE => SeqexecFailure.Unexpected("Bias not supported for GNIRS").asLeft
    case _                 => getCCOtherConfig(config)
  }

  // Used for optional keys. If the key is not present, the result is a None. Other extraction errors are passed on.
  private implicit class KeyAttemptToOption[T](v: Either[ExtractFailure, T]) {
    def optionalKey: Either[ExtractFailure, Option[T]] = v match {
      case Right(r)             => r.some.asRight
      case Left(_: KeyNotFound) => none.asRight
      case Left(e)              => e.asLeft
    }
  }

  private def getCCOtherConfig(config: CleanConfig): TrySeq[CCConfig] = (for {
    xdisp   <- config.extractInstAs[CrossDispersed](CROSS_DISPERSED_PROP)
    woll    <- config.extractInstAs[WollastonPrism](WOLLASTON_PRISM_PROP)
    mode    <- getCCMode(config, xdisp, woll)
    slit    <- config.extractInstAs[SlitWidth](SLIT_WIDTH_PROP)
    slitOp = getSlit(slit)
    camera  <- config.extractInstAs[Camera](CAMERA_PROP)
    decker  <- getDecker(config, slit, woll, xdisp)
    wavel   <- config.extractInstAs[Wavelength](CENTRAL_WAVELENGTH_PROP).map(_.doubleValue().nanometers)
    focus   <- getFocus(config)
    filter  <- config.extractInstAs[Filter](FILTER_PROP).optionalKey
    hartm   <- config.extractInstAs[HartmannMask](HARTMANN_MASK_PROP).optionalKey
    filter1 <- getFilter1(filter, hartm, slit, decker)
    filter2 = getFilter2(filter, xdisp)
  } yield Other(mode, camera, decker, filter1, filter2, focus, wavel, slitOp) )
    .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  private def getCCMode(config: CleanConfig, xdispersed: CrossDispersed, woll: WollastonPrism)
  : Either[ExtractFailure, GnirsController.Mode] =
    for {
      acq        <- config.extractInstAs[AcquisitionMirror](ACQUISITION_MIRROR_PROP)
      disperser  <- config.extractInstAs[Disperser](DISPERSER_PROP)
    } yield {
      if(acq === AcquisitionMirror.IN) GnirsController.Acquisition
      else xdispersed match {
        case CrossDispersed.SXD => GnirsController.CrossDisperserS(disperser)
        case CrossDispersed.LXD => GnirsController.CrossDisperserL(disperser)
        case _                  => if(woll === WollastonPrism.YES) GnirsController.Wollaston(disperser)
                                   else GnirsController.Mirror(disperser)
      }
    }

  private def getDecker(config: CleanConfig, slit: SlitWidth, woll: WollastonPrism, xdisp: CrossDispersed)
  : Either[ExtractFailure, GnirsController.Decker] =
    config.extractInstAs[Decker](DECKER_PROP).orElse {
      for {
        pixScale <- config.extractInstAs[PixelScale](PIXEL_SCALE_PROP)
      } yield xdisp match {
        case CrossDispersed.LXD => Decker.LONG_CAM_X_DISP
        case CrossDispersed.SXD => Decker.SHORT_CAM_X_DISP
        case _                  =>
          if (woll === WollastonPrism.YES) Decker.WOLLASTON
          else pixScale match {
            case PixelScale.PS_005 => Decker.LONG_CAM_LONG_SLIT
            case PixelScale.PS_015 =>
              if (slit === SlitWidth.IFU) Decker.IFU
              else Decker.SHORT_CAM_LONG_SLIT
          }
      }
    }

  private def getFilter1(filter: Option[Filter], hartmann: Option[HartmannMask], slit: SlitWidth, decker: Decker):
  Either[ConfigUtilOps.ExtractFailure, GnirsController.Filter1] =
    if(slit === SlitWidth.PUPIL_VIEWER || decker === Decker.PUPIL_VIEWER) GnirsController.Filter1.PupilViewer.asRight
    else {
      val f = filter.map {
        case Filter.H2_plus_ND100X | Filter.H_plus_ND100X => GnirsController.Filter1.ND100X
        case Filter.Y => GnirsController.Filter1.Y_MK
        case Filter.J => GnirsController.Filter1.J_MK
        case Filter.K => GnirsController.Filter1.K_MK
        case _ => GnirsController.Filter1.Open
      }.getOrElse(GnirsController.Filter1.Open)
      val h = hartmann.flatMap {
        case HartmannMask.OUT        => None
        case HartmannMask.LEFT_MASK  => Filter1.LeftMask.some
        case HartmannMask.RIGHT_MASK => Filter1.RightMask.some
      }
      (h, f) match {
        case (None, _)               => f.asRight
        case (Some(x), Filter1.Open) => x.asRight
        case (Some(x), _)            => ContentError(s"Cannot use filter $f and Hartmann mask $x at the same time")
          .asLeft
      }
    }

  private def getFilter2(filter: Option[Filter], xdisp: CrossDispersed): GnirsController.Filter2 =
    filter.map{
      case Filter.ORDER_1        => GnirsController.Filter2Pos.M
      case Filter.ORDER_2        => GnirsController.Filter2Pos.L
      case Filter.ORDER_3        => GnirsController.Filter2Pos.K
      case Filter.ORDER_4        => GnirsController.Filter2Pos.H
      case Filter.ORDER_5        => GnirsController.Filter2Pos.J
      case Filter.ORDER_6        => GnirsController.Filter2Pos.X
      case Filter.X_DISPERSED    => GnirsController.Filter2Pos.XD
      case Filter.H2             => GnirsController.Filter2Pos.H2
      case Filter.H_plus_ND100X  => GnirsController.Filter2Pos.H
      case Filter.H2_plus_ND100X => GnirsController.Filter2Pos.H2
      case Filter.PAH            => GnirsController.Filter2Pos.PAH
      case _                     => GnirsController.Filter2Pos.Open
    }.map(GnirsController.Manual).getOrElse {
      if (xdisp === CrossDispersed.NO)
        GnirsController.Auto
      else GnirsController.Manual(GnirsController.Filter2Pos.XD)
    }

  private def getSlit(slit: SlitWidth): Option[GnirsController.SlitWidth] = slit match {
    case SlitWidth.ACQUISITION  => GnirsController.SlitWidth.Acquisition.some
    case SlitWidth.PINHOLE_1    => GnirsController.SlitWidth.SmallPinhole.some
    case SlitWidth.PINHOLE_3    => GnirsController.SlitWidth.LargePinhole.some
    case SlitWidth.SW_1         => GnirsController.SlitWidth.Slit0_10.some
    case SlitWidth.SW_2         => GnirsController.SlitWidth.Slit0_15.some
    case SlitWidth.SW_3         => GnirsController.SlitWidth.Slit0_20.some
    case SlitWidth.SW_4         => GnirsController.SlitWidth.Slit0_30.some
    case SlitWidth.SW_5         => GnirsController.SlitWidth.Slit0_45.some
    case SlitWidth.SW_6         => GnirsController.SlitWidth.Slit0_68.some
    case SlitWidth.SW_7         => GnirsController.SlitWidth.Slit1_00.some
    case SlitWidth.PUPIL_VIEWER => GnirsController.SlitWidth.PupilViewer.some
    case _                      => None
  }

  private def getFocus(config: CleanConfig): Either[ExtractFailure, GnirsController.Focus] =
    config.extractInstAs[Focus](FOCUS_PROP).flatMap{ v =>
      if (v.toString === FocusSuggestion.BEST_FOCUS.displayValue) GnirsController.Focus.Best.asRight
      else v.toString.parseIntOption.map(GnirsController.Focus.Manual(_).asRight)
        .getOrElse(ContentError(s"Invalid value for focus ($v)").asLeft)
    }

}
