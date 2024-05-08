// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.Applicative
import cats.effect.Sync
import cats.effect.Ref
import cats.syntax.all._
import seqexec.server.CleanConfig
import edu.gemini.spModel.gemini.ghost.{ Ghost => SPGhost }
import seqexec.server.ConfigUtilOps._
import edu.gemini.spModel.gemini.ghost.GhostBinning
import edu.gemini.spModel.gemini.ghost.GhostReadNoiseGain
import java.lang.{ Boolean => JBoolean, Double => JDouble, Integer => JInt }
import seqexec.server.keywords._
import edu.gemini.spModel.target.env.ResolutionMode
import edu.gemini.spModel.obscomp.InstConstants.OBSERVE_TYPE_PROP
import edu.gemini.spModel.obscomp.InstConstants.COADDS_PROP
import seqexec.model.Conditions
import seqexec.model.GhostTargetName
import shapeless.tag
import scala.concurrent.duration._

sealed trait GhostKeywordsReader[F[_]] {
  def basePos: F[Boolean]
  def srifu1: F[String]
  def srifu2: F[String]
  def hrifu1: F[String]
  def hrifu2: F[String]
  def fiberAgitator1Enabled: F[Boolean]
  def fiberAgitator2Enabled: F[Boolean]
  def redCount: F[Option[Int]]
  def redDuration: F[Option[Double]]
  def redCcds: F[Option[String]]
  def redReadMode: F[Option[String]]
  def blueCount: F[Option[Int]]
  def blueDuration: F[Option[Double]]
  def blueCcds: F[Option[String]]
  def blueReadMode: F[Option[String]]
  def exposureDuration: F[Option[Double]]
  def resolutionMode: F[Option[String]]
  def targetMode: F[Option[String]]
  val targetName: F[Option[String]]
  def slitCount: F[Option[Int]]
  def slitDuration: F[Option[Double]]
}

final class DefaultGhostKeywordsReader[F[_]: Applicative]
    extends GhostKeywordsReader[F]
    with GhostTargetName {

  val basePos: F[Boolean]                 = true.pure[F]
  val srifu1: F[String]                   = "".pure[F]
  val srifu2: F[String]                   = "".pure[F]
  val hrifu1: F[String]                   = "".pure[F]
  val hrifu2: F[String]                   = "".pure[F]
  val fiberAgitator1Enabled: F[Boolean]   = false.pure[F]
  val fiberAgitator2Enabled: F[Boolean]   = false.pure[F]
  val redCount: F[Option[Int]]            = intDefault[F].map(_.some)
  val redDuration: F[Option[Double]]      = doubleDefault[F].map(_.some)
  val redCcds: F[Option[String]]          = strDefault[F].map(_.some)
  val redReadMode: F[Option[String]]      = strDefault[F].map(_.some)
  val blueCount: F[Option[Int]]           = intDefault[F].map(_.some)
  val blueDuration: F[Option[Double]]     = doubleDefault[F].map(_.some)
  val blueCcds: F[Option[String]]         = strDefault[F].map(_.some)
  val blueReadMode: F[Option[String]]     = strDefault[F].map(_.some)
  val resolutionMode: F[Option[String]]   = strDefault[F].map(_.some)
  val targetMode: F[Option[String]]       = strDefault[F].map(_.some)
  val targetName: F[Option[String]]       = strDefault[F].map(_.some)
  val slitCount: F[Option[Int]]           = intDefault[F].map(_.some)
  val slitDuration: F[Option[Double]]     = doubleDefault[F].map(_.some)
  val exposureDuration: F[Option[Double]] = doubleDefault[F].map(_.some)
}

object GhostKeywordsReader extends GhostConfigUtil with GhostLUT with GhostTargetName {
  val readMode2String: GhostReadNoiseGain => String = {
    case GhostReadNoiseGain.SLOW_LOW   => "Slow"
    case GhostReadNoiseGain.MEDIUM_LOW => "Medium"
    case GhostReadNoiseGain.FAST_LOW   => "Rapid"
    case GhostReadNoiseGain.FAST_HIGH  => "Bright"
  }

  val resolutionMode2String: ResolutionMode => String = {
    case ResolutionMode.Standard      => "Standard"
    case ResolutionMode.GhostStandard => "Standard"
    case ResolutionMode.GhostHigh     => "High"
    case ResolutionMode.GhostPRV      => "PRV"
  }

  def exposureTime(
    redDuration:  Option[Double],
    redCount:     Option[Int],
    blueDuration: Option[Double],
    blueCount:    Option[Int]
  ): Option[Double] = (redDuration, redCount, blueDuration, blueCount).mapN(
    (redDuration, redCount, blueDuration, blueCount) =>
      (redDuration * redCount).max(blueDuration * blueCount)
  )

  def apply[F[_]: Sync](
    config:     CleanConfig,
    conditions: Ref[F, Conditions]
  ): GhostKeywordsReader[F] = {
    val raExtractor     = raExtractorBase(config)
    val decExtractor    = decExtractorBase(config)
    val defaultKeywords = new DefaultGhostKeywordsReader()

    (for {
      baseRAHMS  <- raExtractor(SPGhost.BASE_RA_HMS)
      baseDecDMS <- decExtractor(SPGhost.BASE_DEC_DMS)
      obsType    <- config.extractObsAs[String](OBSERVE_TYPE_PROP)
      srifu1Name  = extractor[String](config, SPGhost.SRIFU1_NAME)
      srifu2Name  = extractor[String](config, SPGhost.SRIFU2_NAME)
      hrifu1Name  = extractor[String](config, SPGhost.HRIFU1_NAME)
      hrifu2Name  = extractor[String](config, SPGhost.HRIFU2_NAME)

      fiberAgitator1  =
        config.extractInstAs[JBoolean](SPGhost.FIBER_AGITATOR_1).map(_.booleanValue())
      fiberAgitator2  =
        config.extractInstAs[JBoolean](SPGhost.FIBER_AGITATOR_2).map(_.booleanValue())
      blueExposure    =
        config.extractObsAs[JDouble](SPGhost.BLUE_EXPOSURE_TIME_PROP).map(_.doubleValue())
      redExposure     =
        config.extractObsAs[JDouble](SPGhost.RED_EXPOSURE_TIME_PROP).map(_.doubleValue())
      blueExpCount    = config.extractObsAs[JInt](SPGhost.BLUE_EXPOSURE_COUNT_PROP).map(_.intValue())
      redExpCount     = config.extractObsAs[JInt](SPGhost.RED_EXPOSURE_COUNT_PROP).map(_.intValue())
      blueBinning     = config.extractInstAs[GhostBinning](SPGhost.BLUE_BINNING_PROP)
      redBinning      = config.extractInstAs[GhostBinning](SPGhost.RED_BINNING_PROP)
      coAdds          = config.extractObsAs[JInt](COADDS_PROP).map(_.intValue())
      blueExpReadMode =
        config
          .extractInstAs[GhostReadNoiseGain](SPGhost.BLUE_READ_NOISE_GAIN_PROP)
      redExpReadMode  =
        config
          .extractInstAs[GhostReadNoiseGain](SPGhost.RED_READ_NOISE_GAIN_PROP)
      rm              =
        config
          .extractInstAs[ResolutionMode](SPGhost.RESOLUTION_MODE)
      vMag           <-
        config
          .extractInstAs[JDouble](SPGhost.MAG_V_PROP)
          .map(_.doubleValue().some)
          .recoverWith(_ => none.asRight)

      gMag      <-
        config
          .extractInstAs[JDouble](SPGhost.MAG_G_PROP)
          .map(_.doubleValue().some)
          .recoverWith(_ => none.asRight)
      svOverride =
        config
          .extractInstAs[JDouble](SPGhost.SLIT_VIEWING_CAMERA_EXPOSURE_TIME_PROP)
          .map(_.doubleValue)
    } yield new GhostKeywordsReader[F] {
      private val blueConfig =
        (blueBinning,
         blueExposure.map(_.second),
         blueExpCount,
         blueExpReadMode.map(Ghost.gainFromODB)
        ).mapN(ChannelConfig.apply).map(tag[BlueChannel][ChannelConfig])
      private val redConfig  =
        (redBinning, redExposure.map(_.second), redExpCount, redExpReadMode.map(Ghost.gainFromODB))
          .mapN(ChannelConfig.apply)
          .map(tag[RedChannel][ChannelConfig])

      val basePos: F[Boolean]                 = (baseDecDMS.isEmpty && baseRAHMS.isEmpty).pure[F]
      val srifu1: F[String]                   = srifu1Name.getOrElse("    ").pure[F]
      val srifu2: F[String]                   = srifu2Name.getOrElse("    ").pure[F]
      val hrifu1: F[String]                   = hrifu1Name.getOrElse("    ").pure[F]
      val hrifu2: F[String]                   = hrifu2Name.getOrElse("    ").pure[F]
      val fiberAgitator1Enabled: F[Boolean]   = fiberAgitator1.getOrElse(false).pure[F]
      val fiberAgitator2Enabled: F[Boolean]   = fiberAgitator2.getOrElse(false).pure[F]
      val redCount: F[Option[Int]]            =
        redConfig.map(r => calcRedCount(obsType, coAdds.toOption, r)).toOption.pure[F]
      val redDuration: F[Option[Double]]      = redExposure.toOption.pure[F]
      val redCcds: F[Option[String]]          = redBinning.toOption.map(_.displayValue()).pure[F]
      val redReadMode: F[Option[String]]      = redExpReadMode.toOption.map(readMode2String).pure[F]
      val blueCount: F[Option[Int]]           =
        blueConfig.map(b => calcBlueCount(obsType, coAdds.toOption, b)).toOption.pure[F]
      val blueDuration: F[Option[Double]]     = blueExposure.toOption.pure[F]
      val blueCcds: F[Option[String]]         = blueBinning.toOption.map(_.displayValue()).pure[F]
      val blueReadMode: F[Option[String]]     = blueExpReadMode.toOption.map(readMode2String).pure[F]
      val resolutionMode: F[Option[String]]   = rm.toOption.map(resolutionMode2String).pure[F]
      val targetMode: F[Option[String]]       =
        targetModeFromNames(srifu1Name, srifu2Name, hrifu1Name, hrifu2Name).pure[F]
      val targetName: F[Option[String]]       =
        targetNameFromNames(srifu1Name, srifu2Name, hrifu1Name, hrifu2Name).pure[F]
      val slitCount: F[Option[Int]]           =
        if (isScience(obsType)) {
          conditions.get.map { c =>
            (blueConfig.toOption, redConfig.toOption).mapN { (blue, red) =>
              svOverride.toOption
                .map(t =>
                  svOverrideCameraRepeats(FiniteDuration((t * 1000).toLong, MILLISECONDS),
                                          blue,
                                          red
                  )
                )
                .getOrElse(svCameraRepeats(c, vMag.orElse(gMag), blue, red))
            }
          }
        } else
          (blueConfig.toOption, redConfig.toOption)
            .mapN((blueConfig, redConfig) =>
              svCalibSVRepeats(obsType, blueConfig, redConfig, coAdds.toOption)
            )
            .orElse(GhostCalibrationSVRepeat.some)
            .pure[F]
      val slitDuration: F[Option[Double]]     =
        if (isScience(obsType))
          conditions.get.map { c =>
            svOverride.toOption.orElse(svCameraTime(c, vMag.orElse(gMag)).some)
          }
        else
          (svCalibExposureTime(obsType).toMillis / 1000.0).some.pure[F]
      val exposureDuration: F[Option[Double]] =
        exposureTime(redExposure.toOption,
                     redExpCount.toOption,
                     blueExposure.toOption,
                     blueExpCount.toOption
        ).pure[F]
    }).getOrElse(defaultKeywords)
  }
}
