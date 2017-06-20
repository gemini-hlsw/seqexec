package edu.gemini.seqexec.server

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZoneId, LocalDateTime, Instant}

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.seqexec.server.DhsClient.KeywordBag
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.dataflow.GsaAspect.Visibility
import edu.gemini.spModel.dataflow.GsaSequenceEditor.{HEADER_VISIBILITY_KEY, PROPRIETARY_MONTHS_KEY}
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality._
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants._

import scalaz._
import Scalaz._
import scala.collection.breakOut

/**
  * Created by jluhrs on 1/31/17.
  */

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
case class TimingWindowKeywords(start: SeqAction[String], duration: SeqAction[Double], repeat: SeqAction[Int], period: SeqAction[Double])

case class ObsKeywordReaderImpl(config: Config, telescope: String) extends ObsKeywordsReader {
  import ObsKeywordsReader._

  override def getObsType: SeqAction[String] = SeqAction(
    config.getItemValue(new ItemKey(OBSERVE_KEY, OBSERVE_TYPE_PROP)).toString)

  override def getObsClass: SeqAction[String] = SeqAction(
    config.getItemValue(new ItemKey(OBSERVE_KEY, OBS_CLASS_PROP)).toString)

  override def getGemPrgId: SeqAction[String] = SeqAction(
    config.getItemValue(new ItemKey(OCS_KEY, PROGRAMID_PROP)).toString)

  override def getObsId: SeqAction[String] = SeqAction(
    config.getItemValue(new ItemKey(OCS_KEY, OBSERVATIONID_PROP)).toString
  )

  def explainExtractError(e: ExtractFailure): SeqexecFailure =
    SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))

  override def getRequestedAirMassAngle: Map[String, SeqAction[Double]] =
    List(MAX_AIRMASS, MAX_HOUR_ANGLE, MIN_AIRMASS, MIN_HOUR_ANGLE).flatMap { key =>
      val value = config.extract(new ItemKey(OCS_KEY, "obsConditions:" + key)).as[Double].toOption
      value.map(v => key -> SeqAction(v))
    }(breakOut)

  override def getRequestedConditions: Map[String, SeqAction[String]]  =
    List(SB, CC, IQ, WV).flatMap { key =>
      val value: Option[String] = config.extract(new ItemKey(OCS_KEY, "obsConditions:" + key)).as[String].map { d =>
        (d === "100") ? "Any" | s"$d-percentile"
      }.toOption
      value.map(v => key -> SeqAction(v))
    }(breakOut)

  override def getTimingWindows: List[(Int, TimingWindowKeywords)] = {
    def calcDuration(duration: String): NumberFormatException \/ SeqAction[Double] =
      duration.parseDouble.map { d => SeqAction((d < 0) ? d | d / 1000)}.disjunction

    def calcRepeat(repeat: String): NumberFormatException \/ SeqAction[Int] =
      repeat.parseInt.map(SeqAction(_)).disjunction

    def calcPeriod(period: String): NumberFormatException \/ SeqAction[Double] =
      period.parseDouble.map(p => SeqAction(p/1000)).disjunction

    def calcStart(start: String): NumberFormatException \/ SeqAction[String] =
      start.parseLong.map { s =>
        val timeStr = LocalDateTime.ofInstant(Instant.ofEpochMilli(s), ZoneId.of("GMT")).format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        SeqAction(timeStr)
      }.disjunction

    val prefixes = List(TIMING_WINDOW_START, TIMING_WINDOW_DURATION, TIMING_WINDOW_REPEAT, TIMING_WINDOW_PERIOD)
    val windows = for {
      w <- (0 until 99).toList
    } yield {
      // Keys on the ocs use the prefix and the value and they are always Strings
      val keys = prefixes.map(p => f"$p$w")
      keys.map { k =>
        Option(config.getItemValue(new ItemKey(OCS_KEY, "obsConditions:" + k))).map(_.toString)
      }.sequence.collect {
        case start :: duration :: repeat :: period :: Nil =>
          (calcStart(start) |@| calcDuration(duration) |@| calcRepeat(repeat) |@| calcPeriod(period))(TimingWindowKeywords.apply)
          .map(w -> _).toOption
      }.join
    }
    windows.flatten
  }

  override def getDataLabel: SeqAction[String] = SeqAction(
    config.getItemValue(OBSERVE_KEY / DATA_LABEL_PROP).toString
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
        case ConfigUtilOps.KeyNotFound(_)         => StandardGuideOptions.Value.park.right
        case e@ConfigUtilOps.ConversionError(_,_) => e.left
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
          case ConfigUtilOps.KeyNotFound(_) => new Integer(0).right
          case e@ConfigUtilOps.ConversionError(_, _) => e.left
        }.leftMap(explainExtractError)
          .map(v => LocalDate.now(ZoneId.of("GMT")).plusMonths(v.toLong).format(DateTimeFormatter.ISO_LOCAL_DATE)))
    }
    else SeqAction(LocalDate.now(ZoneId.of("GMT")).format(DateTimeFormatter.ISO_LOCAL_DATE))

  val manualDarkValue = "Manual Dark"
  val manualDarkOverride = "Dark"
  override def getObsObject: SeqAction[String] =
    SeqAction.either(config.extract(OBSERVE_KEY / OBJECT_PROP).as[String]
      .map(v => if(v == manualDarkValue) manualDarkOverride else v).leftMap(explainExtractError))

  override def getGeminiQA: SeqAction[String] = SeqAction("UNKNOWN")

  override def getPIReq: SeqAction[String] = SeqAction("UNKNOWN")

  override def getSciBand: SeqAction[Option[Int]] =
    SeqAction(config.extract(OBSERVE_KEY / SCI_BAND).as[Integer].map(_.toInt).toOption)
}

// TODO: Replace Unit by something that can read the state for real
case class StateKeywordsReader(state: Unit) {
  // TODO: "observer" should be the default when not set in state
  def getObserverName: SeqAction[String] = SeqAction("observer")
  def getOperatorName: SeqAction[String] = SeqAction("ssa")
  def getRawImageQuality: SeqAction[String] = SeqAction("UNKNOWN")
  def getRawCloudCover: SeqAction[String] = SeqAction("UNKNOWN")
  def getRawWaterVapor: SeqAction[String] = SeqAction("UNKNOWN")
  def getRawBackgroundLight: SeqAction[String] = SeqAction("UNKNOWN")
}

class StandardHeader(
  hs: DhsClient,
  obsReader: ObsKeywordsReader,
  tcsReader: TcsKeywordsReader,
  gwsReader: GwsKeywordReader,
  stateReader: StateKeywordsReader) extends Header {

  import Header._
  import Header.Implicits._

  override def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit] = {

    val p: SeqAction[Option[Double]] = for {
      xoffOpt <- tcsReader.getXOffset
      yoffOpt <- tcsReader.getYOffset
      iaaOpt <- tcsReader.getInstrumentAA
    } yield for {
      xoff <- xoffOpt
      yoff <- yoffOpt
      iaa <- iaaOpt
    } yield -xoff * Math.cos(Math.toRadians(iaa)) + yoff * Math.sin(Math.toRadians(iaa))

    val q: SeqAction[Option[Double]] = for {
      xoffOpt <- tcsReader.getXOffset
      yoffOpt <- tcsReader.getYOffset
      iaaOpt <- tcsReader.getInstrumentAA
    } yield for {
      xoff <- xoffOpt
      yoff <- yoffOpt
      iaa <- iaaOpt
    } yield -xoff * Math.sin(Math.toRadians(iaa)) - yoff * Math.cos(Math.toRadians(iaa))

    val raoff: SeqAction[Option[Double]] = for {
      poffOpt <- p
      qoffOpt <- q
      ipaOpt <- tcsReader.getInstrumentPA
    } yield for {
      poff <- poffOpt
      qoff <- qoffOpt
      ipa <- ipaOpt
    } yield poff * Math.cos(Math.toRadians(ipa)) + qoff * Math.sin(Math.toRadians(ipa))

    val decoff: SeqAction[Option[Double]] = for {
      poffOpt <- p
      qoffOpt <- q
      ipaOpt <- tcsReader.getInstrumentPA
    } yield for {
      poff <- poffOpt
      qoff <- qoffOpt
      ipa <- ipaOpt
    } yield poff * Math.cos(Math.toRadians(ipa)) + qoff * Math.sin(Math.toRadians(ipa))

    def guiderKeywords(guideWith: SeqAction[StandardGuideOptions.Value], baseName: String, target: TargetKeywordsReader,
                       extras: List[KeywordBag => SeqAction[KeywordBag]]): SeqAction[Unit] = guideWith.flatMap { g =>
      if (g == StandardGuideOptions.Value.guide) sendKeywords(id, inst, hs, List(
        buildDouble(target.getRA.orDefault, baseName + "ARA"),
        buildDouble(target.getDec.orDefault, baseName + "ADEC"),
        buildDouble(target.getRadialVelocity.orDefault, baseName + "ARV"), {
          val x = target.getWavelength.map(_.map(_.length.toAngstroms))
          buildDouble(x.orDefault, baseName + "AWAVEL")
        },
        buildDouble(target.getEpoch.orDefault, baseName + "AEPOCH"),
        buildDouble(target.getEquinox.orDefault, baseName + "AEQUIN"),
        buildString(target.getFrame.orDefault, baseName + "AFRAME"),
        buildString(target.getObjectName.orDefault, baseName + "AOBJEC"),
        buildDouble(target.getProperMotionDec.orDefault, baseName + "APMDEC"),
        buildDouble(target.getProperMotionRA.orDefault, baseName + "APMRA"),
        buildDouble(target.getParallax.orDefault, baseName + "APARAL")
      ) ++ extras)
      else SeqAction(List())
    }

    def standardGuiderKeywords(guideWith: SeqAction[StandardGuideOptions.Value], baseName: String,
                               target: TargetKeywordsReader, extras: List[KeywordBag => SeqAction[KeywordBag]]): SeqAction[Unit] =
      guiderKeywords(guideWith, baseName, target, List(buildDouble(tcsReader.getM2UserFocusOffset.orDefault, baseName + "FOCUS")) ++ extras)

    val pwfs1Keywords = standardGuiderKeywords(obsReader.getPwfs1Guide, "P1", tcsReader.getPwfs1Target,
      List(buildDouble(tcsReader.getPwfs1Freq.orDefault, "P1FREQ")))

    val pwfs2Keywords = standardGuiderKeywords(obsReader.getPwfs2Guide, "P2", tcsReader.getPwfs2Target,
      List(buildDouble(tcsReader.getPwfs2Freq.orDefault, "P2FREQ")))

    val aowfsKeywords = standardGuiderKeywords(obsReader.getAowfsGuide, "AO", tcsReader.getAowfsTarget, List())

    val oiwfsKeywords = guiderKeywords(obsReader.getOiwfsGuide, "OI", tcsReader.getOiwfsTarget,
      List(buildDouble(tcsReader.getOiwfsFreq.orDefault, "OIFREQ")))

    val gwsKeywords = gwsReader.getHealth.flatMap{
      case Some(0) => sendKeywords(id, inst, hs, List(
        buildDouble(gwsReader.getHumidity.orDefault, "HUMIDITY"),
        {
          val x = gwsReader.getTemperature.map(_.map(_.toCelsiusScale))
          buildDouble(x.orDefault, "TAMBIENT")
        },
        {
          val x = gwsReader.getTemperature.map(_.map(_.toFahrenheitScale))
          buildDouble(x.orDefault, "TAMBIEN2")
        },
        {
          val x = gwsReader.getAirPressure.map(_.map(_.toMillimetersOfMercury))
          buildDouble(x.orDefault, "PRESSURE")
        },
        {
          val x = gwsReader.getAirPressure.map(_.map(_.toPascals))
          buildDouble(x.orDefault, "PRESSUR2")
        },
        {
          val x = gwsReader.getDewPoint.map(_.map(_.toCelsiusScale))
          buildDouble(x.orDefault, "DEWPOINT")
        },
        {
          val x = gwsReader.getDewPoint.map(_.map(_.toFahrenheitScale))
          buildDouble(x.orDefault, "DEWPOIN2")
        },
        {
          val x = gwsReader.getWindVelocity.map(_.map(_.toMetersPerSecond))
          buildDouble(x.orDefault, "WINDSPEE")
        },
        {
          val x = gwsReader.getWindVelocity.map(_.map(_.toInternationalMilesPerHour))
          buildDouble(x.orDefault, "WINDSPE2")
        },
        {
          val x = gwsReader.getWindDirection.map(_.map(_.toDegrees))
          buildDouble(x.orDefault, "WINDDIRE")
        }
      ))
      case _       => SeqAction(())
    }

    val obsObject: SeqAction[Option[String]] = for {
      obsType   <- obsReader.getObsType
      obsObject <- obsReader.getObsObject
      tcsObject <- tcsReader.getSourceATarget.getObjectName
    } yield if (obsType == "OBJECT" && obsObject != "Twilight" && obsObject != "Domeflat") tcsObject
            else Some(obsObject)

    val requestedAirMassAngle: SeqAction[Unit] = {
      import ObsKeywordsReader._
      val keys = List(
        "REQMAXAM" -> MAX_AIRMASS,
        "REQMAXHA" -> MAX_HOUR_ANGLE,
        "REQMINAM" -> MIN_AIRMASS,
        "REQMINHA" -> MIN_HOUR_ANGLE)
      val requested = keys.flatMap {
        case (keyword, value) => obsReader.getRequestedAirMassAngle.get(value).map(buildDouble(_, keyword))
      }
      sendKeywords(id, inst, hs, requested)
    }

    val requestedConditions: SeqAction[Unit] = {
      import ObsKeywordsReader._
      val keys = List(
        "REQBG" -> SB,
        "REQCC" -> CC,
        "REQIQ" -> IQ,
        "REQWV" -> WV)
      val requested = keys.flatMap {
        case (keyword, value) => obsReader.getRequestedConditions.get(value).map(buildString(_, keyword))
      }
      sendKeywords(id, inst, hs, requested)
    }

    val timinigWindows: SeqAction[Unit] = {
      val timingWindows = obsReader.getTimingWindows
      val windows = timingWindows.flatMap {
        case (i, tw) =>
          List(
            buildString(tw.start,    f"REQTWS${i + 1}%02d"),
            buildDouble(tw.duration, f"REQTWD${i + 1}%02d"),
            buildInt32(tw.repeat,    f"REQTWN${i + 1}%02d"),
            buildDouble(tw.period,   f"REQTWP${i + 1}%02d"))
      }
      val windowsCount = buildInt32(SeqAction(timingWindows.length), "NUMREQTW")
      sendKeywords(id, inst, hs, windowsCount :: windows)
    }

    def decodeGuide(v: StandardGuideOptions.Value): String = v match {
      case StandardGuideOptions.Value.park   => "parked"
      case StandardGuideOptions.Value.guide  => "guiding"
      case StandardGuideOptions.Value.freeze => "frozen"
    }

    sendKeywords(id, inst, hs, List(
      buildString(obsReader.getObsType, "OBSTYPE"),
      buildString(obsReader.getObsClass, "OBSCLASS"),
      buildString(obsReader.getGemPrgId, "GEMPRGID"),
      buildString(obsReader.getObsId, "obsid"),
      buildString(obsReader.getDataLabel, "DATALAB"),
      buildString(obsReader.getObservatory, "OBSERVAT"),
      buildString(obsReader.getTelescope, "telescope"),
      buildBoolean(obsReader.getHeaderPrivacy, "PROP_MD"),
      buildString(obsReader.getProprietaryMonths, "RELEASE"),
      buildString(obsObject.orDefault, "OBJECT"),
      buildString(obsReader.getGeminiQA, "RAWGEMQA"),
      buildString(obsReader.getPIReq, "RAWPIREQ"),
      buildString(tcsReader.getHourAngle.orDefault, "HA"),
      buildString(tcsReader.getLocalTime.orDefault, "LT"),
      buildString(tcsReader.getTrackingFrame.orDefault, "TRKFRAME"),
      buildDouble(tcsReader.getTrackingDec.orDefault, "DECTRACK"),
      buildDouble(tcsReader.getTrackingRA.orDefault, "RATRACK"),
      buildDouble(tcsReader.getTrackingEpoch.orDefault, "TRKEPOCH"),
      buildString(tcsReader.getSourceATarget.getFrame.orDefault, "FRAME"),
      buildDouble(tcsReader.getSourceATarget.getProperMotionDec.orDefault, "PMDEC"),
      buildDouble(tcsReader.getSourceATarget.getProperMotionRA.orDefault, "PMRA"),
      {
        val x = tcsReader.getSourceATarget.getWavelength.map(_.map(_.length.toAngstroms))
        buildDouble(x.orDefault, "WAVELENG")
      },
      buildDouble(tcsReader.getSourceATarget.getParallax.orDefault, "PARALLAX"),
      buildDouble(tcsReader.getSourceATarget.getRadialVelocity.orDefault, "RADVEL"),
      buildDouble(tcsReader.getSourceATarget.getEpoch.orDefault, "EPOCH"),
      buildDouble(tcsReader.getSourceATarget.getEquinox.orDefault, "EQUINOX"),
      buildDouble(tcsReader.getSourceATarget.getRA.orDefault, "RA"),
      buildDouble(tcsReader.getSourceATarget.getDec.orDefault, "DEC"),
      buildDouble(tcsReader.getTrackingEquinox.orDefault, "TRKEQUIN"),
      buildDouble(tcsReader.getElevation.orDefault, "ELEVATIO"),
      buildDouble(tcsReader.getAzimuth.orDefault, "AZIMUTH"),
      buildDouble(tcsReader.getCRPositionAngle.orDefault, "CRPA"),
      buildString(tcsReader.getUT.orDefault, "UT"),
      buildString(tcsReader.getDate.orDefault, "DATE"),
      buildString(tcsReader.getM2Baffle.orDefault, "M2BAFFLE"),
      buildString(tcsReader.getM2CentralBaffle.orDefault, "M2CENBAF"),
      buildString(tcsReader.getST.orDefault, "ST"),
      buildDouble(tcsReader.getSFRotation.orDefault, "SFRT2"),
      buildDouble(tcsReader.getSFTilt.orDefault, "SFTILT"),
      buildDouble(tcsReader.getSFLinear.orDefault, "SFLINEAR"),
      buildDouble(tcsReader.getInstrumentPA.orDefault, "PA"),
      buildDouble(tcsReader.getInstrumentAA.orDefault, "IAA"),
      buildDouble(tcsReader.getXOffset.orDefault, "XOFFSET"),
      buildDouble(tcsReader.getYOffset.orDefault, "YOFFSET"),
      buildDouble(p.orDefault, "POFFSET"),
      buildDouble(q.orDefault, "QOFFSET"),
      buildDouble(raoff.orDefault, "RAOFFSET"),
      buildDouble(decoff.orDefault, "DECOFFSE"),
      buildDouble(tcsReader.getTrackingRAOffset.orDefault, "RATRGOFF"),
      buildDouble(tcsReader.getTrackingDecOffset.orDefault, "DECTRGOF"),
      buildString(tcsReader.getAOFoldName.orDefault, "AOFOLD"),
      buildString(tcsReader.getCarouselMode.orDefault, "CGUIDMOD"),
      buildString(obsReader.getPwfs1Guide.map(decodeGuide), "PWFS1_ST"),
      buildString(obsReader.getPwfs2Guide.map(decodeGuide), "PWFS2_ST"),
      buildString(obsReader.getOiwfsGuide.map(decodeGuide), "OIWFS_ST"),
      buildString(obsReader.getAowfsGuide.map(decodeGuide), "AOWFS_ST"),
      buildString(stateReader.getObserverName, "OBSERVER"),
      buildString(stateReader.getOperatorName, "SSA"),
      buildString(stateReader.getRawImageQuality, "RAWIQ"),
      buildString(stateReader.getRawCloudCover, "RAWCC"),
      buildString(stateReader.getRawWaterVapor, "RAWWV"),
      buildString(stateReader.getRawBackgroundLight, "RAWBG"),
      buildInt32(obsReader.getSciBand.orDefault, "SCIBAND")
    )) *>
    requestedConditions *>
    requestedAirMassAngle *>
    timinigWindows *>
    pwfs1Keywords *>
    pwfs2Keywords *>
    oiwfsKeywords *>
    aowfsKeywords *>
    gwsKeywords
  }

  override def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit] = sendKeywords(id, inst, hs,
    List(
      buildDouble(tcsReader.getAirMass.orDefault, "AIRMASS"),
      buildDouble(tcsReader.getStartAirMass.orDefault, "AMSTART"),
      buildDouble(tcsReader.getEndAirMass.orDefault, "AMEND")
    ))
}
