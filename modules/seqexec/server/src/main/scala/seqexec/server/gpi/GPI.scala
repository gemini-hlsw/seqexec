// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import cats.data.Reader
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gpi.Gpi._
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import seqexec.model.dhs.ImageFileId
import seqexec.model.Model.{Instrument, Resource}
import seqexec.server.ConfigUtilOps._
import seqexec.server._
import seqexec.server.gpi.GPIController._
import seqexec.server.keywords.{GDSClient, GDSInstrument}
import scala.concurrent.duration._
import squants.time.{Seconds, Time}

final case class GPI[F[_]: Sync](controller: GPIController[F])
    extends InstrumentSystem[F]
    with GDSInstrument {
  override val gdsClient: GDSClient = controller.gdsClient

  override val resource: Resource = Instrument.GPI

  override val sfName: String = "GPI"

  override val contributorName: String = "gpi"

  override val observeControl: InstrumentSystem.ObserveControl =
    InstrumentSystem.Uncontrollable

  override def observe(
      config: Config): SeqObserveF[F, ImageFileId, ObserveCommand.Result] =
    Reader { fileId =>
      controller
        .observe(fileId)
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
    config
      .extract(OBSERVE_KEY / EXPOSURE_TIME_PROP)
      .as[java.lang.Double]
      .map(x => Seconds(x.toDouble))
      .getOrElse(Seconds(360))
}

object GPI {
  val name: String = INSTRUMENT_NAME_PROP

  private def gpiAoFlags(config: Config): Either[ExtractFailure, AOFlags] =
    for {
      useAo      <- config.extract(INSTRUMENT_KEY / USE_AO_PROP).as[java.lang.Boolean]
      useCal     <- config.extract(INSTRUMENT_KEY / USE_CAL_PROP).as[java.lang.Boolean]
      aoOptimize <- config
                      .extract(INSTRUMENT_KEY / AO_OPTIMIZE_PROP)
                      .as[java.lang.Boolean]
      alignFpm   <- config
                      .extract(INSTRUMENT_KEY / ALIGN_FPM_PINHOLE_BIAS_PROP)
                      .as[java.lang.Boolean]
      magH       <- config
                      .extract(INSTRUMENT_KEY / MAG_H_PROP)
                      .as[java.lang.Double]
      magI       <- config
                      .extract(INSTRUMENT_KEY / MAG_I_PROP)
                      .as[java.lang.Double]
    } yield AOFlags(useAo, useCal, aoOptimize, alignFpm, magH, magI)

  private def gpiASU(
      config: Config): Either[ExtractFailure, ArtificialSources] =
    for {
      ir          <- config
                       .extract(INSTRUMENT_KEY / IR_LASER_LAMP_PROP)
                       .as[ArtificialSource]
      vis         <- config
                       .extract(INSTRUMENT_KEY / VISIBLE_LASER_LAMP_PROP)
                       .as[ArtificialSource]
      sc          <- config
                       .extract(INSTRUMENT_KEY / SUPER_CONTINUUM_LAMP_PROP)
                       .as[ArtificialSource]
      attenuation <- config
                       .extract(
                        INSTRUMENT_KEY / ARTIFICIAL_SOURCE_ATTENUATION_PROP)
                       .as[java.lang.Double]
                       .map(_.toDouble)
    } yield ArtificialSources(ir, vis, sc, attenuation)

  private def gpiShutters(config: Config): Either[ExtractFailure, Shutters] =
    for {
      entrance     <- config
                        .extract(INSTRUMENT_KEY / ENTRANCE_SHUTTER_PROP)
                        .as[Shutter]
      calEntrance  <- config
                        .extract(INSTRUMENT_KEY / CAL_ENTRANCE_SHUTTER_PROP)
                        .as[Shutter]
      scienceArm   <- config
                        .extract(INSTRUMENT_KEY / SCIENCE_ARM_SHUTTER_PROP)
                        .as[Shutter]
      referenceArm <- config
                        .extract(INSTRUMENT_KEY / REFERENCE_ARM_SHUTTER_PROP)
                        .as[Shutter]
    } yield Shutters(entrance, calEntrance, scienceArm, referenceArm)

  private def gpiMode(config: Config): Either[ExtractFailure, Either[ObservingMode, NonStandardModeParams]] =
    config
      .extract(INSTRUMENT_KEY / OBSERVING_MODE_PROP)
      .as[ObservingMode].flatMap { mode =>
        if (mode === ObservingMode.NONSTANDARD) {
          for {
            apodizer <- config
                        .extract(INSTRUMENT_KEY / APODIZER_PROP)
                        .as[Apodizer]
            fpm      <- config
                        .extract(INSTRUMENT_KEY / FPM_PROP)
                        .as[FPM]
            lyot     <- config
                        .extract(INSTRUMENT_KEY / LYOT_PROP)
                        .as[Lyot]
            filter   <- config
                        .extract(INSTRUMENT_KEY / FILTER_PROP)
                        .as[Filter]
          } yield NonStandardModeParams(apodizer, fpm, lyot, filter).asRight
        } else mode.asLeft.asRight
      }

  def fromSequenceConfig[F[_]: Sync](config: Config): SeqActionF[F, GPIConfig] =
    EitherT(Sync[F].delay(
      (for {
        adc      <- config.extract(INSTRUMENT_KEY / ADC_PROP).as[Adc]
        exp      <- config
                      .extract(OBSERVE_KEY / EXPOSURE_TIME_PROP)
                      .as[java.lang.Double]
                      .map(x => Duration(x, SECONDS))
        coa      <- config
                      .extract(OBSERVE_KEY / COADDS_PROP)
                      .as[java.lang.Integer]
                      .map(_.toInt)
        mode     <- gpiMode(config)
        pol      <- config.extract(INSTRUMENT_KEY / DISPERSER_PROP).as[Disperser]
        polA     <- config
                      .extract(INSTRUMENT_KEY / HALF_WAVE_PLATE_ANGLE_VALUE_PROP)
                      .as[java.lang.Double]
                      .map(_.toDouble)
        shutters <- gpiShutters(config)
        asu      <- gpiASU(config)
        pc       <- config.extract(INSTRUMENT_KEY / PUPUL_CAMERA_PROP).as[PupilCamera]
        ao       <- gpiAoFlags(config)
      } yield GPIConfig(adc, exp, coa, mode, pol, polA, shutters, asu, pc, ao))
        .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
    ))

}
