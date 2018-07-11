// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.implicits._
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality._
import edu.gemini.spModel.guide.StandardGuideOptions
import seqexec.model.Model.{Conditions, Observer, Operator}
import seqexec.model.dhs.ImageFileId
import seqexec.server.{SeqAction, OcsBuildInfo, sgoEq}
import seqexec.server.tcs.{TargetKeywordsReader, TcsController, TcsKeywordsReader}

// TODO: Replace Unit by something that can read the state for real
final case class StateKeywordsReader(conditions: Conditions, operator: Option[Operator], observer: Option[Observer]) {
  def encodeCondition(c: Int): String = if(c === 100) "Any" else s"$c-percentile"

  // TODO: "observer" should be the default when not set in state
  def getObserverName: SeqAction[String] = SeqAction(observer.map(_.value).filter(_.nonEmpty).getOrElse("observer"))
  def getOperatorName: SeqAction[String] = SeqAction(operator.map(_.value).filter(_.nonEmpty).getOrElse("ssa"))
  def getRawImageQuality: SeqAction[String] = SeqAction(encodeCondition(conditions.iq.toInt))
  def getRawCloudCover: SeqAction[String] = SeqAction(encodeCondition(conditions.cc.toInt))
  def getRawWaterVapor: SeqAction[String] = SeqAction(encodeCondition(conditions.wv.toInt))
  def getRawBackgroundLight: SeqAction[String] = SeqAction(encodeCondition(conditions.sb.toInt))
}

class StandardHeader[A: HeaderProvider](
  inst: A,
  obsReader: ObsKeywordsReader,
  tcsReader: TcsKeywordsReader,
  stateReader: StateKeywordsReader,
  tcsSubsystems: List[TcsController.Subsystem]) extends Header {

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
    iaaOpt  <- tcsReader.getInstrumentAA
  } yield for {
    xoff <- xoffOpt
    yoff <- yoffOpt
    iaa  <- iaaOpt
  } yield -xoff * Math.sin(Math.toRadians(iaa)) - yoff * Math.cos(Math.toRadians(iaa))

  val raoff: SeqAction[Option[Double]] = for {
    poffOpt <- p
    qoffOpt <- q
    ipaOpt  <- tcsReader.getInstrumentPA
  } yield for {
    poff <- poffOpt
    qoff <- qoffOpt
    ipa  <- ipaOpt
  } yield poff * Math.cos(Math.toRadians(ipa)) + qoff * Math.sin(Math.toRadians(ipa))

  val decoff: SeqAction[Option[Double]] = for {
    poffOpt <- p
    qoffOpt <- q
    ipaOpt  <- tcsReader.getInstrumentPA
  } yield for {
    poff <- poffOpt
    qoff <- qoffOpt
    ipa  <- ipaOpt
  } yield poff * Math.cos(Math.toRadians(ipa)) + qoff * Math.sin(Math.toRadians(ipa))


  val obsObject: SeqAction[Option[String]] = for {
    obsType   <- obsReader.getObsType
    obsObject <- obsReader.getObsObject
    tcsObject <- tcsReader.getSourceATarget.getObjectName
  } yield if (obsType === "OBJECT" && obsObject =!= "Twilight" && obsObject =!= "Domeflat") tcsObject
          else Some(obsObject)

  private def decodeGuide(v: StandardGuideOptions.Value): String = v match {
    case StandardGuideOptions.Value.park   => "parked"
    case StandardGuideOptions.Value.guide  => "guiding"
    case StandardGuideOptions.Value.freeze => "frozen"
  }

  private def optTcsKeyword[B](s: TcsController.Subsystem)(v: SeqAction[B])(implicit d:DefaultValue[B]) : SeqAction[B] =
    if(tcsSubsystems.contains(s)) v
    else SeqAction(d.default)

  private def mountTcsKeyword[B](v: SeqAction[B])(implicit d:DefaultValue[B]) = optTcsKeyword[B](TcsController.Subsystem.Mount)(v)(d)

  private def m2TcsKeyword[B](v: SeqAction[B])(implicit d:DefaultValue[B]) = optTcsKeyword[B](TcsController.Subsystem.M2)(v)(d)

  private def sfTcsKeyword[B](v: SeqAction[B])(implicit d:DefaultValue[B]) = optTcsKeyword[B](TcsController.Subsystem.ScienceFold)(v)(d)

  private val baseKeywords = List(
    buildString(SeqAction(OcsBuildInfo.version), "SEQEXVER"),
    buildString(obsObject.orDefault, "OBJECT"),
    buildString(obsReader.getObsType, "OBSTYPE"),
    buildString(obsReader.getObsClass, "OBSCLASS"),
    buildString(obsReader.getGemPrgId, "GEMPRGID"),
    buildString(obsReader.getObsId, "obsid"),
    buildString(obsReader.getDataLabel, "DATALAB"),
    buildString(stateReader.getObserverName, "OBSERVER"),
    buildString(obsReader.getObservatory, "OBSERVAT"),
    buildString(obsReader.getTelescope, "telescope"),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getParallax.orDefault), "PARALLAX"),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getRadialVelocity.orDefault), "RADVEL"),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getEpoch.orDefault), "EPOCH"),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getEquinox.orDefault), "EQUINOX"),
    buildDouble(mountTcsKeyword(tcsReader.getTrackingEquinox.orDefault), "TRKEQUIN"),
    buildString(stateReader.getOperatorName, "SSA"),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getRA.orDefault), "RA"),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getDec.orDefault), "DEC"),
    buildDouble(tcsReader.getElevation.orDefault, "ELEVATIO"),
    buildDouble(tcsReader.getAzimuth.orDefault, "AZIMUTH"),
    buildDouble(mountTcsKeyword(tcsReader.getCRPositionAngle.orDefault), "CRPA"),
    buildString(tcsReader.getHourAngle.orDefault, "HA"),
    buildString(tcsReader.getLocalTime.orDefault, "LT"),
    buildString(mountTcsKeyword(tcsReader.getTrackingFrame.orDefault), "TRKFRAME"),
    buildDouble(mountTcsKeyword(tcsReader.getTrackingDec.orDefault), "DECTRACK"),
    buildDouble(mountTcsKeyword(tcsReader.getTrackingEpoch.orDefault), "TRKEPOCH"),
    buildDouble(mountTcsKeyword(tcsReader.getTrackingRA.orDefault), "RATRACK"),
    buildString(mountTcsKeyword(tcsReader.getSourceATarget.getFrame.orDefault), "FRAME"),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getProperMotionDec.orDefault), "PMDEC"),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getProperMotionRA.orDefault), "PMRA"),
    {
      val x = tcsReader.getSourceATarget.getWavelength.map(_.map(_.length.toAngstroms))
      buildDouble(mountTcsKeyword(x.orDefault), "WAVELENG")
    },
    buildString(stateReader.getRawImageQuality, "RAWIQ"),
    buildString(stateReader.getRawCloudCover, "RAWCC"),
    buildString(stateReader.getRawWaterVapor, "RAWWV"),
    buildString(stateReader.getRawBackgroundLight, "RAWBG"),
    buildString(obsReader.getPIReq, "RAWPIREQ"),
    buildString(obsReader.getGeminiQA, "RAWGEMQA"),
    buildString(tcsReader.getCarouselMode.orDefault, "CGUIDMOD"),
    buildString(tcsReader.getUT.orDefault, "UT"),
    buildString(tcsReader.getDate.orDefault, "DATE"),
    buildString(m2TcsKeyword(tcsReader.getM2Baffle.orDefault), "M2BAFFLE"),
    buildString(m2TcsKeyword(tcsReader.getM2CentralBaffle.orDefault), "M2CENBAF"),
    buildString(tcsReader.getST.orDefault, "ST"),
    buildDouble(mountTcsKeyword(tcsReader.getXOffset.orDefault), "XOFFSET"),
    buildDouble(mountTcsKeyword(tcsReader.getYOffset.orDefault), "YOFFSET"),
    buildDouble(mountTcsKeyword(p.orDefault), "POFFSET"),
    buildDouble(mountTcsKeyword(q.orDefault), "QOFFSET"),
    buildDouble(mountTcsKeyword(raoff.orDefault), "RAOFFSET"),
    buildDouble(mountTcsKeyword(decoff.orDefault), "DECOFFSE"),
    buildDouble(mountTcsKeyword(tcsReader.getTrackingRAOffset.orDefault), "RATRGOFF"),
    buildDouble(mountTcsKeyword(tcsReader.getTrackingDecOffset.orDefault), "DECTRGOF"),
    buildDouble(mountTcsKeyword(tcsReader.getInstrumentPA.orDefault), "PA"),
    buildDouble(mountTcsKeyword(tcsReader.getInstrumentAA.orDefault), "IAA"),
    buildDouble(sfTcsKeyword(tcsReader.getSFRotation.orDefault), "SFRT2"),
    buildDouble(sfTcsKeyword(tcsReader.getSFTilt.orDefault), "SFTILT"),
    buildDouble(sfTcsKeyword(tcsReader.getSFLinear.orDefault), "SFLINEAR"),
    buildString(mountTcsKeyword(tcsReader.getAOFoldName.orDefault), "AOFOLD"),
    buildString(obsReader.getPwfs1Guide.map(decodeGuide), "PWFS1_ST"),
    buildString(obsReader.getPwfs2Guide.map(decodeGuide), "PWFS2_ST"),
    buildString(obsReader.getOiwfsGuide.map(decodeGuide), "OIWFS_ST"),
    buildString(obsReader.getAowfsGuide.map(decodeGuide), "AOWFS_ST"),
    buildInt32(obsReader.getSciBand.orDefault, "SCIBAND")
  )

  def timinigWindows(id: ImageFileId): SeqAction[Unit] = {
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
    sendKeywords(id, inst, windowsCount :: windows)
  }

  def requestedConditions(id: ImageFileId): SeqAction[Unit] = {
    import ObsKeywordsReader._
    val keys = List(
      "REQIQ" -> IQ,
      "REQCC" -> CC,
      "REQBG" -> SB,
      "REQWV" -> WV)
    val requested = keys.flatMap {
      case (keyword, value) => obsReader.getRequestedConditions.get(value).toList.map(buildString(_, keyword))
    }
    sendKeywords(id, inst, requested)
  }

  def requestedAirMassAngle(id: ImageFileId): SeqAction[Unit] = {
    import ObsKeywordsReader._
    val keys = List(
      "REQMAXAM" -> MAX_AIRMASS,
      "REQMAXHA" -> MAX_HOUR_ANGLE,
      "REQMINAM" -> MIN_AIRMASS,
      "REQMINHA" -> MIN_HOUR_ANGLE)
    val requested = keys.flatMap {
      case (keyword, value) => obsReader.getRequestedAirMassAngle.get(value).toList.map(buildDouble(_, keyword))
    }
    if (!requested.isEmpty) sendKeywords(id, inst, requested)
    else SeqAction.void
  }

  // scalastyle:of
  override def sendBefore(id: ImageFileId): SeqAction[Unit] = {
    def guiderKeywords(guideWith: SeqAction[StandardGuideOptions.Value], baseName: String, target: TargetKeywordsReader,
                       extras: List[KeywordBag => SeqAction[KeywordBag]]): SeqAction[Unit] = guideWith.flatMap { g =>
      if (g === StandardGuideOptions.Value.guide) sendKeywords(id, inst, List(
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
      else SeqAction.void
    }

    def standardGuiderKeywords(guideWith: SeqAction[StandardGuideOptions.Value], baseName: String,
                               target: TargetKeywordsReader, extras: List[KeywordBag => SeqAction[KeywordBag]]): SeqAction[Unit] =
      guiderKeywords(guideWith, baseName, target, List(buildDouble(tcsReader.getM2UserFocusOffset.orDefault, baseName + "FOCUS")) ++ extras)

    val oiwfsKeywords = guiderKeywords(obsReader.getOiwfsGuide, "OI", tcsReader.getOiwfsTarget,
      List(buildDouble(tcsReader.getOiwfsFreq.orDefault, "OIFREQ")))

    val pwfs1Keywords = standardGuiderKeywords(obsReader.getPwfs1Guide, "P1", tcsReader.getPwfs1Target,
      List(buildDouble(tcsReader.getPwfs1Freq.orDefault, "P1FREQ")))

    val pwfs2Keywords = standardGuiderKeywords(obsReader.getPwfs2Guide, "P2", tcsReader.getPwfs2Target,
      List(buildDouble(tcsReader.getPwfs2Freq.orDefault, "P2FREQ")))

    val aowfsKeywords = standardGuiderKeywords(obsReader.getAowfsGuide, "AO", tcsReader.getAowfsTarget, List())

    sendKeywords(id, inst, baseKeywords) *>
    requestedConditions(id) *>
    requestedAirMassAngle(id) *>
    timinigWindows(id) *>
    pwfs1Keywords *>
    pwfs2Keywords *>
    oiwfsKeywords *>
    aowfsKeywords
  }
  // scalastyle:on

  override def sendAfter(id: ImageFileId): SeqAction[Unit] = sendKeywords(id, inst,
    List(
      buildDouble(tcsReader.getAirMass.orDefault, "AIRMASS"),
      buildDouble(tcsReader.getStartAirMass.orDefault, "AMSTART"),
      buildDouble(tcsReader.getEndAirMass.orDefault, "AMEND"),
      buildBoolean(obsReader.getHeaderPrivacy, "PROP_MD"),
      buildString(obsReader.getProprietaryMonths, "RELEASE")
    ))
}
