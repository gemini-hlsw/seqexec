// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._
import seqexec.server.CleanConfig
import edu.gemini.spModel.gemini.ghost.{ Ghost => SPGhost }
import seqexec.server.ConfigUtilOps._
import edu.gemini.spModel.gemini.ghost.GhostBinning
import java.lang.{ Boolean => JBoolean, Double => JDouble, Integer => JInt }
import seqexec.server.keywords._

sealed trait GhostKeywordsReader[F[_]] {
  def basePos: F[Boolean]
  def srifu1: F[String]
  def srifu2: F[String]
  def hrifu1: F[String]
  def hrifu2: F[String]
  def fiberAgitator1Enabled: F[Boolean]
  def fiberAgitator2Enabled: F[Boolean]
  def ifu1Guiding: F[Boolean]
  def ifu2Guiding: F[Boolean]
  def redCount: F[Option[Int]]
  def redDuration: F[Option[Double]]
  def redCcds: F[Option[String]]
  def blueCount: F[Option[Int]]
  def blueDuration: F[Option[Double]]
  def blueCcds: F[Option[String]]
  // def slitCount: F[Int]
  // def slitDuration: F[Double]
}

final class DefaultGhostKeywordsReader[F[_]: Applicative] extends GhostKeywordsReader[F] {
  val basePos: F[Boolean]               = true.pure[F]
  val srifu1: F[String]                 = "".pure[F]
  val srifu2: F[String]                 = "".pure[F]
  val hrifu1: F[String]                 = "".pure[F]
  val hrifu2: F[String]                 = "".pure[F]
  val fiberAgitator1Enabled: F[Boolean] = false.pure[F]
  val fiberAgitator2Enabled: F[Boolean] = false.pure[F]
  val ifu1Guiding: F[Boolean]           = false.pure[F]
  val ifu2Guiding: F[Boolean]           = false.pure[F]
  val redCount: F[Option[Int]]          = intDefault[F].map(_.some)
  val redDuration: F[Option[Double]]    = doubleDefault[F].map(_.some)
  val redCcds: F[Option[String]]        = strDefault[F].map(_.some)
  val blueCount: F[Option[Int]]         = intDefault[F].map(_.some)
  val blueDuration: F[Option[Double]]   = doubleDefault[F].map(_.some)
  val blueCcds: F[Option[String]]       = strDefault[F].map(_.some)
  // val slitCount: F[Int]                 = intDefault[F]
  // val slitDuration: F[Double]           = doubleDefault[F]
}

object GhostKeywordsReader extends GhostConfigUtil {
  def apply[F[_]: Sync](config: CleanConfig): GhostKeywordsReader[F] = {
    val raExtractor     = raExtractorBase(config)
    val decExtractor    = decExtractorBase(config)
    val defaultKeywords = new DefaultGhostKeywordsReader()

    (for {
      baseRAHMS    <- raExtractor(SPGhost.BASE_RA_HMS)
      baseDecDMS   <- decExtractor(SPGhost.BASE_DEC_DMS)
      srifu1Name    = extractor[String](config, SPGhost.SRIFU1_NAME)
      srifu2Name    = extractor[String](config, SPGhost.SRIFU2_NAME)
      hrifu1Name    = extractor[String](config, SPGhost.HRIFU1_NAME)
      hrifu2Name    = extractor[String](config, SPGhost.HRIFU2_NAME)
      srifu1Guiding = extractor[JBoolean](config, SPGhost.SRIFU1_GUIDING)
                        .orElse(
                          extractor[JBoolean](config, SPGhost.HRIFU1_GUIDING)
                        )
                        .map(_.booleanValue())
      srifu2Guiding = extractor[JBoolean](config, SPGhost.SRIFU2_GUIDING).map(_.booleanValue())

      fiberAgitator1 =
        config.extractInstAs[JBoolean](SPGhost.FIBER_AGITATOR_1).map(_.booleanValue())
      fiberAgitator2 =
        config.extractInstAs[JBoolean](SPGhost.FIBER_AGITATOR_2).map(_.booleanValue())
      blueExposure   =
        config.extractObsAs[JDouble](SPGhost.BLUE_EXPOSURE_TIME_PROP).map(_.doubleValue())
      redExposure    =
        config.extractObsAs[JDouble](SPGhost.RED_EXPOSURE_TIME_PROP).map(_.doubleValue())
      blueExpCount   = config.extractObsAs[JInt](SPGhost.BLUE_EXPOSURE_COUNT_PROP).map(_.intValue())
      redExpCount    = config.extractObsAs[JInt](SPGhost.RED_EXPOSURE_COUNT_PROP).map(_.intValue())
      blueBinning    = config.extractInstAs[GhostBinning](SPGhost.BLUE_BINNING_PROP)
      redBinning     = config.extractInstAs[GhostBinning](SPGhost.RED_BINNING_PROP)
    } yield new GhostKeywordsReader[F] {
      val basePos: F[Boolean]               = (baseDecDMS.isEmpty && baseRAHMS.isEmpty).pure[F]
      val srifu1: F[String]                 = srifu1Name.getOrElse("    ").pure[F]
      val srifu2: F[String]                 = srifu2Name.getOrElse("    ").pure[F]
      val hrifu1: F[String]                 = hrifu1Name.getOrElse("    ").pure[F]
      val hrifu2: F[String]                 = hrifu2Name.getOrElse("    ").pure[F]
      val fiberAgitator1Enabled: F[Boolean] = fiberAgitator1.getOrElse(false).pure[F]
      val fiberAgitator2Enabled: F[Boolean] = fiberAgitator2.getOrElse(false).pure[F]
      val ifu1Guiding: F[Boolean]           = srifu1Guiding.getOrElse(false).pure[F]
      val ifu2Guiding: F[Boolean]           = srifu2Guiding.getOrElse(false).pure[F]
      val redCount: F[Option[Int]]          = redExpCount.toOption.pure[F]
      val redDuration: F[Option[Double]]    = redExposure.toOption.pure[F]
      val blueCount: F[Option[Int]]         = blueExpCount.toOption.pure[F]
      val blueDuration: F[Option[Double]]   = blueExposure.toOption.pure[F]
      val redCcds: F[Option[String]]        = redBinning.toOption.map(_.displayValue()).pure[F]
      val blueCcds: F[Option[String]]       = blueBinning.toOption.map(_.displayValue()).pure[F]
      // val slitCount: F[Int]
      // val slitDuration: F[Double]
    }).getOrElse(defaultKeywords)
  }
}
