// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import cats.data.Kleisli
import cats.data.EitherT
import cats.effect.{Concurrent, Sync, Timer}
import cats.implicits._
import fs2.Stream
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.{Reads, _}
import edu.gemini.spModel.obscomp.InstConstants.{DARK_OBSERVE_TYPE, OBSERVE_TYPE_PROP}
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import java.lang.{Double => JDouble}

import gem.enum.LightSinkName
import io.chrisdavenport.log4cats.Logger

import scala.concurrent.duration.{Duration, SECONDS}
import seqexec.server.tcs.FOCAL_PLANE_SCALE
import seqexec.model.enum.Instrument
import seqexec.model.enum.ObserveCommandResult
import seqexec.model.dhs.ImageFileId
import seqexec.server.ConfigUtilOps._
import seqexec.server.flamingos2.Flamingos2Controller._
import seqexec.server._
import seqexec.server.keywords.{DhsClient, DhsInstrument, KeywordsClient}
import squants.Length
import squants.space.Arcseconds
import squants.time.{Seconds, Time}

import scala.reflect.ClassTag

final case class Flamingos2[F[_]: Timer: Logger: Concurrent](
  f2Controller: Flamingos2Controller[F],
  dhsClient: DhsClient[F]
) extends DhsInstrument[F] with InstrumentSystem[F] {

  import Flamingos2._

  override val resource: Instrument = Instrument.F2

  override def sfName(config: CleanConfig): LightSinkName = LightSinkName.F2

  override val contributorName: String = "flamingos2"

  override val dhsInstrumentName: String = "F2"

  override val keywordsClient: KeywordsClient[F] = this

  override def observeControl(config: CleanConfig): InstrumentSystem.ObserveControl[F] = InstrumentSystem.Uncontrollable

  // FLAMINGOS-2 does not support abort or stop.
  override def observe(config: CleanConfig): Kleisli[F, ImageFileId, ObserveCommandResult] =
    Kleisli { fileId =>
      calcObserveTime(config).flatMap { x =>
        f2Controller.observe(fileId, x)
      }
  }

  override def configure(config: CleanConfig): F[ConfigResult[F]] =
    EitherT.fromEither[F](fromSequenceConfig(config))
      .widenRethrowT
      .flatMap(f2Controller.applyConfig)
      .as(ConfigResult(this))

  override def notifyObserveEnd: F[Unit] =
    f2Controller.endObserve

  override def notifyObserveStart: F[Unit] = Sync[F].unit

  override def calcObserveTime(config: CleanConfig): F[Time] =
    Sync[F].delay(
      config.extractObsAs[JDouble](EXPOSURE_TIME_PROP)
        .map(x => Seconds(x.toDouble)).getOrElse(Seconds(360)))

  override def observeProgress(total: Time, elapsed: InstrumentSystem.ElapsedTime): Stream[F, Progress] =
    f2Controller.observeProgress(total)

  override def instrumentActions(config: CleanConfig): InstrumentActions[F] =
    InstrumentActions.defaultInstrumentActions[F]

  // TODO Use different value if using electronic offsets
  override val oiOffsetGuideThreshold: Option[Length] =
    (Arcseconds(0.01)/FOCAL_PLANE_SCALE).some
}

object Flamingos2 {
  val name: String = INSTRUMENT_NAME_PROP

  def fpuFromFPUnit(fpu: FPUnit): FocalPlaneUnit = fpu match {
    case FPUnit.FPU_NONE       => FocalPlaneUnit.Open
    case FPUnit.SUBPIX_PINHOLE => FocalPlaneUnit.GridSub1Pix
    case FPUnit.PINHOLE        => FocalPlaneUnit.Grid2Pix
    case FPUnit.LONGSLIT_1     => FocalPlaneUnit.Slit1Pix
    case FPUnit.LONGSLIT_2     => FocalPlaneUnit.Slit2Pix
    case FPUnit.LONGSLIT_3     => FocalPlaneUnit.Slit3Pix
    case FPUnit.LONGSLIT_4     => FocalPlaneUnit.Slit4Pix
    case FPUnit.LONGSLIT_6     => FocalPlaneUnit.Slit6Pix
    case FPUnit.LONGSLIT_8     => FocalPlaneUnit.Slit8Pix
    case FPUnit.CUSTOM_MASK    => FocalPlaneUnit.Custom("")
  }

  def readsFromReadMode(readMode: ReadMode): Flamingos2Controller.Reads = readMode match {
    case ReadMode.BRIGHT_OBJECT_SPEC => Reads.READS_1
    case ReadMode.MEDIUM_OBJECT_SPEC => Reads.READS_4
    case ReadMode.FAINT_OBJECT_SPEC  => Reads.READS_8
  }

  implicit def biasFromDecker(dk: Decker): BiasMode = dk match {
    case Decker.IMAGING   => BiasMode.Imaging
    case Decker.LONG_SLIT => BiasMode.LongSlit
    case Decker.MOS       => BiasMode.MOS
  }

  def fpuConfig(config: CleanConfig): Either[ConfigUtilOps.ExtractFailure, FocalPlaneUnit] = {
    val a = FPU_PROP
    val b = FPU_MASK_PROP

    config.extractInstAs[FPUnit](a).flatMap(x =>
      if(x =!= FPUnit.CUSTOM_MASK) fpuFromFPUnit(x).asRight
      else config.extractInstAs[String](b).map(FocalPlaneUnit.Custom)
    )
  }

  def windowCoverFromObserveType(observeType: String): WindowCover = observeType match {
    case DARK_OBSERVE_TYPE    => WindowCover.CLOSE
    case _                    => WindowCover.OPEN
  }

  implicit def grismFromSPDisperser(d: Disperser): Grism = d match {
    case Disperser.NONE    => Grism.Open
    case Disperser.R1200HK => Grism.R1200HK
    case Disperser.R1200JH => Grism.R1200JH
    case Disperser.R3000   => Grism.R3000
  }

  private def disperserFromObserveType(observeType: String, d: Disperser): Grism = observeType match {
    case DARK_OBSERVE_TYPE    => Grism.Dark
    case _                    => d
  }

  // This method deals with engineering parameters that can come as a T or an Option[T]
  private def extractEngineeringParam[T](item: Extracted[CleanConfig], default: T)(
    implicit clazz: ClassTag[T]): Either[ExtractFailure, T] = item.as[T].recoverWith {
      case _:ConfigUtilOps.KeyNotFound     => Right(default)
      case _:ConfigUtilOps.ConversionError => item.as[edu.gemini.shared.util.immutable.Option[T]]
                                                .map(_.getOrElse(default))
    }

  def ccConfigFromSequenceConfig(config: CleanConfig): TrySeq[CCConfig] =
    (for {
      obsType <- config.extractObsAs[String](OBSERVE_TYPE_PROP)
      // WINDOW_COVER_PROP is optional. It can be a WindowCover, an Option[WindowCover], or not be present. If no
      // value is given, then window cover position is inferred from observe type.
      p <- extractEngineeringParam(config.extract(INSTRUMENT_KEY / WINDOW_COVER_PROP),
             windowCoverFromObserveType(obsType)
           )
      q <- config.extractInstAs[Decker](DECKER_PROP)
      r <- fpuConfig(config)
      f <- config.extractInstAs[Filter](FILTER_PROP)
      s <- if(f.isObsolete) ContentError(s"Obsolete filter ${f.displayValue}").asLeft
           else f.asRight
      t <- config.extractInstAs[LyotWheel](LYOT_WHEEL_PROP)
      u <- config.extractInstAs[Disperser](DISPERSER_PROP).map(disperserFromObserveType(obsType, _))
    } yield CCConfig(p, q, r, s, t, u)).leftMap(e => SeqexecFailure.Unexpected
    (ConfigUtilOps.explain(e)))

  def dcConfigFromSequenceConfig(config: CleanConfig): TrySeq[DCConfig] =
    (for {
      p <- config.extractObsAs[JDouble](EXPOSURE_TIME_PROP).map(x => Duration(x, SECONDS))
      // Reads is usually inferred from the read mode, but it can be explicit.
      a <- config.extractInstAs[ReadMode](READMODE_PROP).map(readsFromReadMode)
      q <- extractEngineeringParam(config.extract(OBSERVE_KEY / READS_PROP), a)
      // Readout mode defaults to SCIENCE if not present.
      r <- extractEngineeringParam(config.extract(INSTRUMENT_KEY / READOUT_MODE_PROP), ReadoutMode.SCIENCE)
      s <- config.extractInstAs[Decker](DECKER_PROP)
    } yield DCConfig(p, q, r, s)).leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))

  def fromSequenceConfig[F[_]: Sync](config: CleanConfig): Either[SeqexecFailure, Flamingos2Config] = ( for {
      p <- ccConfigFromSequenceConfig(config)
      q <- dcConfigFromSequenceConfig(config)
    } yield Flamingos2Config(p, q)
   )

}
