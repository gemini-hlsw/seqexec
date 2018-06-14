// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import seqexec.server.ConfigUtilOps._
import seqexec.server._
import seqexec.server.gpi.GPIController._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gpi.Gpi._
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import scala.concurrent.duration._
import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._

object GPI {
  val name: String = INSTRUMENT_NAME_PROP

  val sfName: String = GPI.sfName

  private def gpiAoFlags(config: Config): Either[ExtractFailure, AOFlags] =
    for {
      useAo      <- config.extract(INSTRUMENT_KEY / USE_AO_PROP).as[Boolean]
      useCal     <- config.extract(INSTRUMENT_KEY / USE_CAL_PROP).as[Boolean]
      aoOptimize <- config
                      .extract(INSTRUMENT_KEY / AO_OPTIMIZE_PROP)
                      .as[Boolean]
      alignFpm   <- config
                      .extract(INSTRUMENT_KEY / ALIGN_FPM_PINHOLE_BIAS_PROP)
                      .as[Boolean]
    } yield AOFlags(useAo, useCal, aoOptimize, alignFpm)

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
        mode     <- config
                      .extract(INSTRUMENT_KEY / OBSERVING_MODE_PROP)
                      .as[ObservingMode]
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
