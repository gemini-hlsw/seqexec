// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats.data.EitherT
import cats.effect.{IO, Sync}
import cats.implicits._
import cats.data.Reader
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gpi.Gpi._
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import java.lang.{Boolean => JBoolean, Double => JDouble, Integer => JInt}
import seqexec.model.dhs.ImageFileId
import seqexec.model.Model.{Instrument, Resource}
import seqexec.server.ConfigUtilOps._
import seqexec.server._
import seqexec.server.gpi.GPIController._
import seqexec.server.keywords.{GDSClient, GDSInstrument, KeywordsClient}
import scala.concurrent.duration._
import squants.time.{Milliseconds, Seconds, Time}

final case class GPI[F[_]: Sync](controller: GPIController[F])
    extends InstrumentSystem[F]
    with GDSInstrument {
  override val gdsClient: GDSClient = controller.gdsClient

  override val keywordsClient: KeywordsClient[IO] = this

  override val resource: Resource = Instrument.GPI

  override val sfName: String = "GPI"

  override val contributorName: String = "gpi"

  override val observeControl: InstrumentSystem.ObserveControl =
    InstrumentSystem.Uncontrollable

  override def observe(
      config: Config): SeqObserveF[F, ImageFileId, ObserveCommand.Result] =
    Reader { fileId =>
      controller
        .observe(fileId, calcObserveTime(config))
        .map(_ => ObserveCommand.Success: ObserveCommand.Result)
    }

  override def configure(config: Config): SeqActionF[F, ConfigResult[F]] =
    GPI
      .fromSequenceConfig[F](config)
      .flatMap(controller.applyConfig)
      .map(_ => ConfigResult(this))

  override def notifyObserveEnd: SeqActionF[F, Unit] = controller.endObserve

  override def notifyObserveStart: SeqActionF[F, Unit] = SeqActionF.void

  override def calcObserveTime(config: Config): Time =
    (for {
     exp      <- config.extractAs[JDouble](OBSERVE_KEY / EXPOSURE_TIME_PROP)
     coa      <- config.extractAs[JInt](OBSERVE_KEY / COADDS_PROP).map(_.toInt)
     } yield Seconds(2.2 * exp * coa + 300)).getOrElse(Milliseconds(100))
}

object GPI {
  val name: String = INSTRUMENT_NAME_PROP

  private def gpiAoFlags(config: Config): Either[ExtractFailure, AOFlags] =
    for {
      useAo      <- config.extractAs[JBoolean](INSTRUMENT_KEY / USE_AO_PROP)
      useCal     <- config.extractAs[JBoolean](INSTRUMENT_KEY / USE_CAL_PROP)
      aoOptimize <- config.extractAs[JBoolean](INSTRUMENT_KEY / AO_OPTIMIZE_PROP)
      alignFpm   <- config.extractAs[JBoolean](INSTRUMENT_KEY / ALIGN_FPM_PINHOLE_BIAS_PROP)
      magH       <- config.extractAs[JDouble](INSTRUMENT_KEY / MAG_H_PROP)
      magI       <- config.extractAs[JDouble](INSTRUMENT_KEY / MAG_I_PROP)
    } yield AOFlags(useAo, useCal, aoOptimize, alignFpm, magH, magI)

  private def gpiASU(
      config: Config): Either[ExtractFailure, ArtificialSources] =
    for {
      ir          <- config.extractAs[ArtificialSource](INSTRUMENT_KEY / IR_LASER_LAMP_PROP)
      vis         <- config.extractAs[ArtificialSource](INSTRUMENT_KEY / VISIBLE_LASER_LAMP_PROP)
      sc          <- config.extractAs[ArtificialSource](INSTRUMENT_KEY / SUPER_CONTINUUM_LAMP_PROP)
      attenuation <- config.extractAs[JDouble](INSTRUMENT_KEY / ARTIFICIAL_SOURCE_ATTENUATION_PROP)
                       .map(_.toDouble)
    } yield ArtificialSources(ir, vis, sc, attenuation)

  private def gpiShutters(config: Config): Either[ExtractFailure, Shutters] =
    for {
      entrance     <- config.extractAs[Shutter](INSTRUMENT_KEY / ENTRANCE_SHUTTER_PROP)
      calEntrance  <- config.extractAs[Shutter](INSTRUMENT_KEY / CAL_ENTRANCE_SHUTTER_PROP)
      scienceArm   <- config.extractAs[Shutter](INSTRUMENT_KEY / SCIENCE_ARM_SHUTTER_PROP)
      referenceArm <- config.extractAs[Shutter](INSTRUMENT_KEY / REFERENCE_ARM_SHUTTER_PROP)
    } yield Shutters(entrance, calEntrance, scienceArm, referenceArm)

  private def gpiMode(config: Config): Either[ExtractFailure, Either[ObservingMode, NonStandardModeParams]] =
    config
      .extractAs[ObservingMode](INSTRUMENT_KEY / OBSERVING_MODE_PROP)
      .flatMap { mode =>
        if (mode === ObservingMode.NONSTANDARD) {
          for {
            apodizer <- config.extractAs[Apodizer](INSTRUMENT_KEY / APODIZER_PROP)
            fpm      <- config.extractAs[FPM](INSTRUMENT_KEY / FPM_PROP)
            lyot     <- config.extractAs[Lyot](INSTRUMENT_KEY / LYOT_PROP)
            filter   <- config.extractAs[Filter](INSTRUMENT_KEY / FILTER_PROP)
          } yield NonStandardModeParams(apodizer, fpm, lyot, filter).asRight
        } else mode.asLeft.asRight
      }

  def fromSequenceConfig[F[_]: Sync](config: Config): SeqActionF[F, GPIConfig] =
    EitherT(Sync[F].delay(
      (for {
        adc      <- config.extractAs[Adc](INSTRUMENT_KEY / ADC_PROP)
        exp      <- config.extractAs[JDouble](OBSERVE_KEY / EXPOSURE_TIME_PROP)
                      .map(x => Duration(x, SECONDS))
        coa      <- config.extractAs[JInt](OBSERVE_KEY / COADDS_PROP)
                      .map(_.toInt)
        mode     <- gpiMode(config)
        pol      <- config.extractAs[Disperser](INSTRUMENT_KEY / DISPERSER_PROP)
        polA     <- config.extractAs[JDouble](INSTRUMENT_KEY / HALF_WAVE_PLATE_ANGLE_VALUE_PROP)
                      .map(_.toDouble)
        shutters <- gpiShutters(config)
        asu      <- gpiASU(config)
        pc       <- config.extractAs[PupilCamera](INSTRUMENT_KEY / PUPUL_CAMERA_PROP)
        ao       <- gpiAoFlags(config)
      } yield GPIConfig(adc, exp, coa, mode, pol, polA, shutters, asu, pc, ao))
        .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
    ))

}
