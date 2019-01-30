// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.Show
import cats.implicits._
import cats.effect.Sync
import gem.enum.Site
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.dataflow.GsaAspect.Visibility
import edu.gemini.spModel.dataflow.GsaSequenceEditor.{HEADER_VISIBILITY_KEY, PROPRIETARY_MONTHS_KEY}
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality._
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants._
import edu.gemini.spModel.gemini.gpi.Gpi.ASTROMETRIC_FIELD_PROP
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}
import mouse.all._
import scala.collection.breakOut
import seqexec.server.ConfigUtilOps._
import seqexec.server.tcs.Tcs
import seqexec.server.{ConfigUtilOps, SeqActionF, SeqexecFailure}

trait ObsKeywordsReader[F[_]] {
  def getObsType: SeqActionF[F, String]
  def getObsClass: SeqActionF[F, String]
  def getGemPrgId: SeqActionF[F, String]
  def getObsId: SeqActionF[F, String]
  def getDataLabel: SeqActionF[F, String]
  def getObservatory: SeqActionF[F, String]
  def getTelescope: SeqActionF[F, String]
  def getPwfs1Guide: SeqActionF[F, StandardGuideOptions.Value]
  def getPwfs2Guide: SeqActionF[F, StandardGuideOptions.Value]
  def getOiwfsGuide: SeqActionF[F, StandardGuideOptions.Value]
  def getAowfsGuide: SeqActionF[F, StandardGuideOptions.Value]
  def getHeaderPrivacy: SeqActionF[F, Boolean]
  def getProprietaryMonths: SeqActionF[F, String]
  def getObsObject: SeqActionF[F, String]
  def getGeminiQA: SeqActionF[F, String]
  def getPIReq: SeqActionF[F, String]
  def getSciBand: SeqActionF[F, Option[Int]]
  def getRequestedAirMassAngle: Map[String, SeqActionF[F, Double]]
  def getTimingWindows: List[(Int, TimingWindowKeywords[F])]
  def getRequestedConditions: Map[String, SeqActionF[F, String]]
  def getAstrometicField: SeqActionF[F, Boolean]
}

object ObsKeywordsReader {
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

// A Timing window always have 4 keywords
final case class TimingWindowKeywords[F[_]](start: SeqActionF[F, String], duration: SeqActionF[F, Double], repeat: SeqActionF[F, Int], period: SeqActionF[F, Double])

final case class ObsKeywordReaderImpl[F[_]: Sync](config: Config, site: Site) extends ObsKeywordsReader[F] {
  // Format used on FITS keywords
  val telescope: String = site match {
    case Site.GN => "Gemini-North"
    case Site.GS => "Gemini-South"
  }
  import ObsKeywordsReader._

  implicit private val show: Show[AnyRef] = Show.fromToString

  override def getObsType: SeqActionF[F, String] = SeqActionF(
    config.getItemValue(new ItemKey(OBSERVE_KEY, OBSERVE_TYPE_PROP)).show)

  override def getObsClass: SeqActionF[F, String] = SeqActionF(
    config.getItemValue(new ItemKey(OBSERVE_KEY, OBS_CLASS_PROP)).show)

  override def getGemPrgId: SeqActionF[F, String] = SeqActionF(
    config.getItemValue(new ItemKey(OCS_KEY, PROGRAMID_PROP)).show)

  override def getObsId: SeqActionF[F, String] = SeqActionF(
    config.getItemValue(new ItemKey(OCS_KEY, OBSERVATIONID_PROP)).show)

  def explainExtractError(e: ExtractFailure): SeqexecFailure =
    SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))

  override def getRequestedAirMassAngle: Map[String, SeqActionF[F, Double]] =
    List(MAX_AIRMASS, MAX_HOUR_ANGLE, MIN_AIRMASS, MIN_HOUR_ANGLE).flatMap { key =>
      val value = config.extractAs[Double](new ItemKey(OCS_KEY, "obsConditions:" + key)).toOption
      value.toList.map(v => key -> SeqActionF(v))
    }(breakOut)

  override def getRequestedConditions: Map[String, SeqActionF[F, String]]  =
    List(SB, CC, IQ, WV).flatMap { key =>
      val value: Option[String] = config.extractAs[String](new ItemKey(OCS_KEY, "obsConditions:" + key)).map { d =>
        (d === "100").fold("Any", s"$d-percentile")
      }.toOption
      value.toList.map(v => key -> SeqActionF(v))
    }(breakOut)

  override def getTimingWindows: List[(Int, TimingWindowKeywords[F])] = {
    def calcDuration(duration: String): Either[NumberFormatException, SeqActionF[F, Double]] =
      duration.parseDouble.map { d => SeqActionF((d < 0).fold(d, d / 1000))}

    def calcRepeat(repeat: String): Either[NumberFormatException, SeqActionF[F, Int]] =
      repeat.parseInt.map(SeqActionF(_))

    def calcPeriod(period: String): Either[NumberFormatException, SeqActionF[F, Double]] =
      period.parseDouble.map(p => SeqActionF(p/1000))

    def calcStart(start: String): Either[NumberFormatException, SeqActionF[F, String]] =
      start.parseLong.map { s =>
        val timeStr = LocalDateTime.ofInstant(Instant.ofEpochMilli(s), ZoneId.of("GMT")).format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        SeqActionF(timeStr)
      }

    val prefixes = List(TIMING_WINDOW_START, TIMING_WINDOW_DURATION, TIMING_WINDOW_REPEAT, TIMING_WINDOW_PERIOD)
    val windows = for {
      w <- (0 until 99).toList
    } yield {
      // Keys on the ocs use the prefix and the value and they are always Strings
      val keys = prefixes.map(p => f"$p$w")
      keys.map { k =>
        Option(config.getItemValue(new ItemKey(OCS_KEY, "obsConditions:" + k))).map(_.show)
      }.sequence.collect {
        case start :: duration :: repeat :: period :: Nil =>
          (calcStart(start), calcDuration(duration), calcRepeat(repeat), calcPeriod(period)).mapN(TimingWindowKeywords.apply)
          .map(w -> _).toOption
      }.collect { case Some(x) => x }
    }
    windows.collect { case Some(x) => x }
  }

  override def getDataLabel: SeqActionF[F, String] = SeqActionF(
    config.getItemValue(OBSERVE_KEY / DATA_LABEL_PROP).show
  )

  override def getObservatory: SeqActionF[F, String] = SeqActionF(telescope)

  override def getTelescope: SeqActionF[F, String] = SeqActionF(telescope)

  override def getPwfs1Guide: SeqActionF[F, StandardGuideOptions.Value] =
    SeqActionF.either(config.extractAs[StandardGuideOptions.Value](new ItemKey(TELESCOPE_KEY, Tcs.GUIDE_WITH_PWFS1_PROP))
      .leftMap(explainExtractError))

  override def getPwfs2Guide: SeqActionF[F, StandardGuideOptions.Value] =
    SeqActionF.either(config.extractAs[StandardGuideOptions.Value](new ItemKey(TELESCOPE_KEY, Tcs.GUIDE_WITH_PWFS2_PROP))
      .leftMap(explainExtractError))

  override def getOiwfsGuide: SeqActionF[F, StandardGuideOptions.Value] =
    SeqActionF.either(config.extractAs[StandardGuideOptions.Value](new ItemKey(TELESCOPE_KEY, GUIDE_WITH_OIWFS_PROP))
      .recoverWith[ConfigUtilOps.ExtractFailure, StandardGuideOptions.Value] {
        case ConfigUtilOps.KeyNotFound(_)         => StandardGuideOptions.Value.park.asRight
        case e@ConfigUtilOps.ConversionError(_,_) => e.asLeft
      }.leftMap(explainExtractError))

  override def getAowfsGuide: SeqActionF[F, StandardGuideOptions.Value] =
    SeqActionF.either(config.extractAs[StandardGuideOptions.Value](new ItemKey(TELESCOPE_KEY, Tcs.GUIDE_WITH_AOWFS_PROP))
      .recoverWith[ConfigUtilOps.ExtractFailure, StandardGuideOptions.Value] {
        case ConfigUtilOps.KeyNotFound(_)         => StandardGuideOptions.Value.park.asRight
        case e@ConfigUtilOps.ConversionError(_,_) => e.asLeft
      }.leftMap(explainExtractError))

  private val headerPrivacy: Boolean = config.extractAs[Visibility](HEADER_VISIBILITY_KEY).getOrElse(Visibility.PUBLIC) match {
    case Visibility.PRIVATE => true
    case _                  => false
  }

  override def getHeaderPrivacy: SeqActionF[F, Boolean] = SeqActionF(headerPrivacy)

  override def getProprietaryMonths: SeqActionF[F, String] =
    if(headerPrivacy) {
      SeqActionF.either(
        config.extractAs[Integer](PROPRIETARY_MONTHS_KEY).recoverWith[ConfigUtilOps.ExtractFailure, Integer]{
          case ConfigUtilOps.KeyNotFound(_) => new Integer(0).asRight
          case e@ConfigUtilOps.ConversionError(_, _) => e.asLeft
        }.leftMap(explainExtractError)
          .map(v => LocalDate.now(ZoneId.of("GMT")).plusMonths(v.toLong).format(DateTimeFormatter.ISO_LOCAL_DATE)))
    }
    else SeqActionF(LocalDate.now(ZoneId.of("GMT")).format(DateTimeFormatter.ISO_LOCAL_DATE))

  private val manualDarkValue = "Manual Dark"
  private val manualDarkOverride = "Dark"
  override def getObsObject: SeqActionF[F, String] =
    SeqActionF.either(config.extractAs[String](OBSERVE_KEY / OBJECT_PROP)
      .map(v => if(v === manualDarkValue) manualDarkOverride else v).leftMap(explainExtractError))

  override def getGeminiQA: SeqActionF[F, String] = SeqActionF("UNKNOWN")

  override def getPIReq: SeqActionF[F, String] = SeqActionF("UNKNOWN")

  override def getSciBand: SeqActionF[F, Option[Int]] =
    SeqActionF(config.extractAs[Integer](OBSERVE_KEY / SCI_BAND).map(_.toInt).toOption)

  def getAstrometicField: SeqActionF[F, Boolean] =
    SeqActionF.either(config.extractAs[java.lang.Boolean](INSTRUMENT_KEY / ASTROMETRIC_FIELD_PROP)
      .leftMap(explainExtractError)).map(Boolean.unbox)
}
