// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.implicits._
import cats.{MonadError, Eq}
import cats.data.EitherT
import cats.data.Nested
import edu.gemini.spModel.dataflow.GsaAspect.Visibility
import edu.gemini.spModel.dataflow.GsaSequenceEditor.{HEADER_VISIBILITY_KEY, PROPRIETARY_MONTHS_KEY}
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality._
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants._
import edu.gemini.spModel.gemini.gpi.Gpi.ASTROMETRIC_FIELD_PROP
import gem.enum.Site
import gsp.math.syntax.string._
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}

import edu.gemini.spModel.gemini.gems.Canopus
import edu.gemini.spModel.gemini.gsaoi.GsaoiOdgw
import mouse.boolean._
import seqexec.server.{CleanConfig, ConfigUtilOps, SeqexecFailure}
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigUtilOps._
import seqexec.server.tcs.Tcs

sealed trait ObsKeywordsReader[F[_]] {
  def obsType: F[String]
  def obsClass: F[String]
  def gemPrgId: F[String]
  def obsId: F[String]
  def dataLabel: F[String]
  def observatory: F[String]
  def telescope: F[String]
  def pwfs1Guide: F[StandardGuideOptions.Value]
  def pwfs1GuideS: F[String]
  def pwfs2Guide: F[StandardGuideOptions.Value]
  def pwfs2GuideS: F[String]
  def oiwfsGuide: F[StandardGuideOptions.Value]
  def oiwfsGuideS: F[String]
  def aowfsGuide: F[StandardGuideOptions.Value]
  def aowfsGuideS: F[String]
  def cwfs1Guide: F[StandardGuideOptions.Value]
  def cwfs2Guide: F[StandardGuideOptions.Value]
  def cwfs3Guide: F[StandardGuideOptions.Value]
  def odgw1Guide: F[StandardGuideOptions.Value]
  def odgw2Guide: F[StandardGuideOptions.Value]
  def odgw3Guide: F[StandardGuideOptions.Value]
  def odgw4Guide: F[StandardGuideOptions.Value]
  def headerPrivacy: F[Boolean]
  def proprietaryMonths: F[String]
  def obsObject: F[String]
  def geminiQA: F[String]
  def pIReq: F[String]
  def sciBand: F[Int]
  def requestedAirMassAngle: F[Map[String, Double]]
  def timingWindows: F[List[(Int, TimingWindowKeywords)]]
  def requestedConditions: F[Map[String, String]]
  def astrometicField: F[Boolean]
}

trait ObsKeywordsReaderConstants {
  // Constants taken from SPSiteQualityCB
  // TODO Make them public in SPSiteQualityCB
  val MIN_HOUR_ANGLE: String         = "MinHourAngle"
  val MAX_HOUR_ANGLE: String         = "MaxHourAngle"
  val MIN_AIRMASS: String            = "MinAirmass"
  val MAX_AIRMASS: String            = "MaxAirmass"

  val TIMING_WINDOW_START: String    = "TimingWindowStart"
  val TIMING_WINDOW_DURATION: String = "TimingWindowDuration"
  val TIMING_WINDOW_REPEAT: String   = "TimingWindowRepeat"
  val TIMING_WINDOW_PERIOD: String   = "TimingWindowPeriod"

  val SB: String = SKY_BACKGROUND_PROP.getName
  val CC: String = CLOUD_COVER_PROP.getName
  val IQ: String = IMAGE_QUALITY_PROP.getName
  val WV: String = WATER_VAPOR_PROP.getName
}

// A Timing window always has 4 keywords
final case class TimingWindowKeywords(
  start: String,
  duration: Double,
  repeat: Int,
  period: Double
)

object ObsKeywordReader extends ObsKeywordsReaderConstants {
  def apply[F[_]: MonadError[?[_], Throwable]](config: CleanConfig, site: Site): ObsKeywordsReader[F] = new ObsKeywordsReader[F] {
    // Format used on FITS keywords
    val telescopeName: String = site match {
      case Site.GN => "Gemini-North"
      case Site.GS => "Gemini-South"
    }

    override def obsType: F[String] = EitherT(
      config.extractObsAs[String](OBSERVE_TYPE_PROP)
        .leftMap(explainExtractError)
        .pure[F]
    ).widenRethrowT

    override def obsClass: F[String] = EitherT(
      config.extractObsAs[String](OBS_CLASS_PROP)
        .leftMap(explainExtractError)
        .pure[F]
    ).widenRethrowT

    override def gemPrgId: F[String] = EitherT(
      config.extractAs[String](OCS_KEY / PROGRAMID_PROP)
        .leftMap(explainExtractError)
        .pure[F]
    ).widenRethrowT

    override def obsId: F[String] = EitherT(
      config.extractAs[String](OCS_KEY / OBSERVATIONID_PROP)
        .leftMap(explainExtractError)
        .pure[F]
    ).widenRethrowT

    private def explainExtractError(e: ExtractFailure): SeqexecFailure =
      SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))

    private val ObsConditionsProp = "obsConditions"

    override def requestedAirMassAngle: F[Map[String, Double]] = {
      val keys: F[List[Option[(String, Double)]]] =
        List(MAX_AIRMASS, MAX_HOUR_ANGLE, MIN_AIRMASS, MIN_HOUR_ANGLE).map { key =>
          val value: F[Option[Double]] =
            config.extractAs[String](OCS_KEY / ObsConditionsProp / key)
              .toOption
              .flatMap(_.parseDoubleOption)
              .pure[F]

          Nested(value).map(key -> _).value
          .handleError(_ => none) // If there is an error ignore the key
        }.sequence
      keys.map {_.mapFilter(identity).toMap}
    }

    override def requestedConditions: F[Map[String, String]] = {
      val keys: F[List[(String, String)]] =
        List(SB, CC, IQ, WV).map { key =>
          val value: F[String] =
            config.extractAs[String](OCS_KEY / ObsConditionsProp / key)
              .map { d => (d === "100").fold("Any", s"$d-percentile") }
              .toOption
              .pure[F]
              .safeValOrDefault
          value.map(key -> _)
        }.sequence
      keys.map(_.toMap)
    }

    override def timingWindows: F[List[(Int, TimingWindowKeywords)]] = {
      def calcDuration(duration: String): Option[Double] =
        duration.parseDoubleOption.map { d => (d < 0).fold(d, d / 1000) }

      def calcRepeat(repeat: String): Option[Int] =
        repeat.parseIntOption

      def calcPeriod(period: String): Option[Double] =
        period.parseDoubleOption.map(p => p/1000)

      def calcStart(start: String): Option[String] =
        start.parseLongOption.map { s =>
          LocalDateTime.ofInstant(Instant.ofEpochMilli(s),
            ZoneId.of("GMT")).format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        }

      val prefixes =
        List(
          TIMING_WINDOW_START,
          TIMING_WINDOW_DURATION,
          TIMING_WINDOW_REPEAT,
          TIMING_WINDOW_PERIOD)
      val windows = for {
        w <- (0 until 99).toList
      } yield {
        // Keys on the ocs use the prefix and the value and they are always Strings
        val keys = prefixes.map(p => f"$p$w")
        keys.map { k =>
          config.extractAs[String](OCS_KEY / ObsConditionsProp / k).getOrElse("")
        } match {
          case start :: duration :: repeat :: period :: Nil =>
            (calcStart(start), calcDuration(duration), calcRepeat(repeat), calcPeriod(period))
              .mapN(TimingWindowKeywords.apply)
              .map(w -> _)
          case _ => none
        }
      }
      windows.flattenOption.pure[F]
    }

    override def dataLabel: F[String] = EitherT(
      config.extractObsAs[String](DATA_LABEL_PROP)
        .leftMap(explainExtractError)
        .pure[F]
    ).widenRethrowT

    override def observatory: F[String] = telescopeName.pure[F]

    override def telescope: F[String] = telescopeName.pure[F]

    private def decodeGuide(v: StandardGuideOptions.Value): String = v match {
      case StandardGuideOptions.Value.park   => "parked"
      case StandardGuideOptions.Value.guide  => "guiding"
      case StandardGuideOptions.Value.freeze => "frozen"
    }

    override def pwfs1Guide: F[StandardGuideOptions.Value] =
      EitherT(
        config.extractTelescopeAs[StandardGuideOptions.Value](Tcs.GUIDE_WITH_PWFS1_PROP)
          .leftMap(explainExtractError)
          .pure[F]
      ).widenRethrowT

    override def pwfs1GuideS: F[String] =
      pwfs1Guide
        .map(decodeGuide)

    override def pwfs2Guide: F[StandardGuideOptions.Value] =
      EitherT(
        config.extractTelescopeAs[StandardGuideOptions.Value](Tcs.GUIDE_WITH_PWFS2_PROP)
          .leftMap(explainExtractError)
          .pure[F]
      ).widenRethrowT

    override def pwfs2GuideS: F[String] =
      pwfs2Guide
        .map(decodeGuide)

    private def extractOptionalGuide(prop: String): F[StandardGuideOptions.Value] =
      EitherT(
        config.extractTelescopeAs[StandardGuideOptions.Value](prop)
        .recoverWith {
          case ConfigUtilOps.KeyNotFound(_) => StandardGuideOptions.Value.park.asRight
        }
        .leftMap(explainExtractError)
        .pure[F]
      ).widenRethrowT

    override def oiwfsGuide: F[StandardGuideOptions.Value] = extractOptionalGuide(GUIDE_WITH_OIWFS_PROP)

    override def oiwfsGuideS: F[String] =
      oiwfsGuide
        .map(decodeGuide)

    override def aowfsGuide: F[StandardGuideOptions.Value] = extractOptionalGuide(Tcs.GUIDE_WITH_AOWFS_PROP)

    override def aowfsGuideS: F[String] =
      aowfsGuide
        .map(decodeGuide)

    override def cwfs1Guide: F[StandardGuideOptions.Value] = extractOptionalGuide(Canopus.Wfs.cwfs1.getSequenceProp)

    override def cwfs2Guide: F[StandardGuideOptions.Value] = extractOptionalGuide(Canopus.Wfs.cwfs2.getSequenceProp)

    override def cwfs3Guide: F[StandardGuideOptions.Value] = extractOptionalGuide(Canopus.Wfs.cwfs3.getSequenceProp)

    override def odgw1Guide: F[StandardGuideOptions.Value] = extractOptionalGuide(GsaoiOdgw.odgw1.getSequenceProp)

    override def odgw2Guide: F[StandardGuideOptions.Value] = extractOptionalGuide(GsaoiOdgw.odgw2.getSequenceProp)

    override def odgw3Guide: F[StandardGuideOptions.Value] = extractOptionalGuide(GsaoiOdgw.odgw3.getSequenceProp)

    override def odgw4Guide: F[StandardGuideOptions.Value] = extractOptionalGuide(GsaoiOdgw.odgw4.getSequenceProp)

    private implicit val eqVisibility: Eq[Visibility] = Eq.by(_.ordinal())

    private val headerPrivacyF: F[Boolean] =
      (config.extractAs[Visibility](HEADER_VISIBILITY_KEY).getOrElse(Visibility.PUBLIC) === Visibility.PRIVATE)
      .pure[F]


    override def headerPrivacy: F[Boolean] = headerPrivacyF

    private val noProprietaryMonths: F[String] =
      LocalDate.now(ZoneId.of("GMT")).format(DateTimeFormatter.ISO_LOCAL_DATE).pure[F]

    private val calcProprietaryMonths: F[String] =
      EitherT(
        config.extractAs[Integer](PROPRIETARY_MONTHS_KEY)
          .recoverWith {
            case ConfigUtilOps.KeyNotFound(_) => new Integer(0).asRight
          }
          .leftMap(explainExtractError)
          .map { v =>
            LocalDate.now(ZoneId.of("GMT")).plusMonths(v.toLong).format(DateTimeFormatter.ISO_LOCAL_DATE)
          }
          .pure[F]
        )
      .widenRethrowT

    override def proprietaryMonths: F[String] =
      headerPrivacyF
        .ifM(calcProprietaryMonths, noProprietaryMonths)

    private val manualDarkValue = "Manual Dark"
    private val manualDarkOverride = "Dark"

    override def obsObject: F[String] =
      config.extractObsAs[String](OBJECT_PROP)
        .map(v => if (v === manualDarkValue) manualDarkOverride else v)
        .toOption
        .pure[F]
        .safeValOrDefault

    override def geminiQA: F[String] = "UNKNOWN".pure[F]

    override def pIReq: F[String] = "UNKNOWN".pure[F]

    override def sciBand: F[Int] =
      config.extractObsAs[Integer](SCI_BAND)
      .map(_.toInt)
      .toOption
      .pure[F]
      .safeValOrDefault

    def astrometicField: F[Boolean] =
      config.extractInstAs[java.lang.Boolean](ASTROMETRIC_FIELD_PROP)
      .toOption
      .exists(Boolean.unbox)
      .pure[F]

  }
}
