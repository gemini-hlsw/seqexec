// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats._
import cats.implicits._
import gem.enum.Site
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.dataflow.GsaAspect.Visibility
import edu.gemini.spModel.dataflow.GsaSequenceEditor.{HEADER_VISIBILITY_KEY, PROPRIETARY_MONTHS_KEY}
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality._
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants._
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}
import mouse.all._
import scala.collection.breakOut
import seqexec.server.ConfigUtilOps._
import seqexec.server.tcs.Tcs
import seqexec.server.{ConfigUtilOps, SeqAction, SeqexecFailure}

trait ObsKeywordsReader {
  def getObsType: SeqAction[String]
  def getObsClass: SeqAction[String]
  def getGemPrgId: SeqAction[String]
  def getObsId: SeqAction[String]
  def getDataLabel: SeqAction[String]
  def getObservatory: SeqAction[String]
  def getTelescope: SeqAction[String]
  def getPwfs1Guide: SeqAction[StandardGuideOptions.Value]
  def getPwfs2Guide: SeqAction[StandardGuideOptions.Value]
  def getOiwfsGuide: SeqAction[StandardGuideOptions.Value]
  def getAowfsGuide: SeqAction[StandardGuideOptions.Value]
  def getHeaderPrivacy: SeqAction[Boolean]
  def getProprietaryMonths: SeqAction[String]
  def getObsObject: SeqAction[String]
  def getGeminiQA: SeqAction[String]
  def getPIReq: SeqAction[String]
  def getSciBand: SeqAction[Option[Int]]
  def getRequestedAirMassAngle: Map[String, SeqAction[Double]]
  def getTimingWindows: List[(Int, TimingWindowKeywords)]
  def getRequestedConditions: Map[String, SeqAction[String]]
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
final case class TimingWindowKeywords(start: SeqAction[String], duration: SeqAction[Double], repeat: SeqAction[Int], period: SeqAction[Double])

final case class ObsKeywordReaderImpl(config: Config, site: Site) extends ObsKeywordsReader {
  // Format used on FITS keywords
  val telescope: String = site match {
    case Site.GN => "Gemini-North"
    case Site.GS => "Gemini-South"
  }
  import ObsKeywordsReader._

  implicit private val show: Show[AnyRef] = Show.fromToString

  override def getObsType: SeqAction[String] = SeqAction(
    config.getItemValue(new ItemKey(OBSERVE_KEY, OBSERVE_TYPE_PROP)).show)

  override def getObsClass: SeqAction[String] = SeqAction(
    config.getItemValue(new ItemKey(OBSERVE_KEY, OBS_CLASS_PROP)).show)

  override def getGemPrgId: SeqAction[String] = SeqAction(
    config.getItemValue(new ItemKey(OCS_KEY, PROGRAMID_PROP)).show)

  override def getObsId: SeqAction[String] = SeqAction(
    config.getItemValue(new ItemKey(OCS_KEY, OBSERVATIONID_PROP)).show)

  def explainExtractError(e: ExtractFailure): SeqexecFailure =
    SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))

  override def getRequestedAirMassAngle: Map[String, SeqAction[Double]] =
    List(MAX_AIRMASS, MAX_HOUR_ANGLE, MIN_AIRMASS, MIN_HOUR_ANGLE).flatMap { key =>
      val value = config.extract(new ItemKey(OCS_KEY, "obsConditions:" + key)).as[Double].toOption
      value.toList.map(v => key -> SeqAction(v))
    }(breakOut)

  override def getRequestedConditions: Map[String, SeqAction[String]]  =
    List(SB, CC, IQ, WV).flatMap { key =>
      val value: Option[String] = config.extract(new ItemKey(OCS_KEY, "obsConditions:" + key)).as[String].map { d =>
        (d === "100").fold("Any", s"$d-percentile")
      }.toOption
      value.toList.map(v => key -> SeqAction(v))
    }(breakOut)

  override def getTimingWindows: List[(Int, TimingWindowKeywords)] = {
    def calcDuration(duration: String): Either[NumberFormatException, SeqAction[Double]] =
      duration.parseDouble.map { d => SeqAction((d < 0).fold(d, d / 1000))}

    def calcRepeat(repeat: String): Either[NumberFormatException, SeqAction[Int]] =
      repeat.parseInt.map(SeqAction(_))

    def calcPeriod(period: String): Either[NumberFormatException, SeqAction[Double]] =
      period.parseDouble.map(p => SeqAction(p/1000))

    def calcStart(start: String): Either[NumberFormatException, SeqAction[String]] =
      start.parseLong.map { s =>
        val timeStr = LocalDateTime.ofInstant(Instant.ofEpochMilli(s), ZoneId.of("GMT")).format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        SeqAction(timeStr)
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

  override def getDataLabel: SeqAction[String] = SeqAction(
    config.getItemValue(OBSERVE_KEY / DATA_LABEL_PROP).show
  )

  override def getObservatory: SeqAction[String] = SeqAction(telescope)

  override def getTelescope: SeqAction[String] = SeqAction(telescope)

  override def getPwfs1Guide: SeqAction[StandardGuideOptions.Value] =
    SeqAction.either(config.extract(new ItemKey(TELESCOPE_KEY, Tcs.GUIDE_WITH_PWFS1_PROP)).as[StandardGuideOptions.Value]
      .leftMap(explainExtractError))

  override def getPwfs2Guide: SeqAction[StandardGuideOptions.Value] =
    SeqAction.either(config.extract(new ItemKey(TELESCOPE_KEY, Tcs.GUIDE_WITH_PWFS2_PROP)).as[StandardGuideOptions.Value]
      .leftMap(explainExtractError))

  override def getOiwfsGuide: SeqAction[StandardGuideOptions.Value] =
    SeqAction.either(config.extract(new ItemKey(TELESCOPE_KEY, GUIDE_WITH_OIWFS_PROP)).as[StandardGuideOptions.Value]
      .leftMap(explainExtractError))

  override def getAowfsGuide: SeqAction[StandardGuideOptions.Value] =
    SeqAction.either(config.extract(new ItemKey(TELESCOPE_KEY, Tcs.GUIDE_WITH_AOWFS_PROP)).as[StandardGuideOptions.Value]
      .recoverWith[ConfigUtilOps.ExtractFailure, StandardGuideOptions.Value] {
        case ConfigUtilOps.KeyNotFound(_)         => StandardGuideOptions.Value.park.asRight
        case e@ConfigUtilOps.ConversionError(_,_) => e.asLeft
      }.leftMap(explainExtractError))

  private val headerPrivacy: Boolean = config.extract(HEADER_VISIBILITY_KEY).as[Visibility].getOrElse(Visibility.PUBLIC) match {
    case Visibility.PRIVATE => true
    case _                  => false
  }

  override def getHeaderPrivacy: SeqAction[Boolean] = SeqAction(headerPrivacy)

  override def getProprietaryMonths: SeqAction[String] =
    if(headerPrivacy) {
      SeqAction.either(
        config.extract(PROPRIETARY_MONTHS_KEY).as[Integer].recoverWith[ConfigUtilOps.ExtractFailure, Integer]{
          case ConfigUtilOps.KeyNotFound(_) => new Integer(0).asRight
          case e@ConfigUtilOps.ConversionError(_, _) => e.asLeft
        }.leftMap(explainExtractError)
          .map(v => LocalDate.now(ZoneId.of("GMT")).plusMonths(v.toLong).format(DateTimeFormatter.ISO_LOCAL_DATE)))
    }
    else SeqAction(LocalDate.now(ZoneId.of("GMT")).format(DateTimeFormatter.ISO_LOCAL_DATE))

  private val manualDarkValue = "Manual Dark"
  private val manualDarkOverride = "Dark"
  override def getObsObject: SeqAction[String] =
    SeqAction.either(config.extract(OBSERVE_KEY / OBJECT_PROP).as[String]
      .map(v => if(v === manualDarkValue) manualDarkOverride else v).leftMap(explainExtractError))

  override def getGeminiQA: SeqAction[String] = SeqAction("UNKNOWN")

  override def getPIReq: SeqAction[String] = SeqAction("UNKNOWN")

  override def getSciBand: SeqAction[Option[Int]] =
    SeqAction(config.extract(OBSERVE_KEY / SCI_BAND).as[Integer].map(_.toInt).toOption)
}
