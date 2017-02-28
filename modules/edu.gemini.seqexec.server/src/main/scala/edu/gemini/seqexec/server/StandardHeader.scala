package edu.gemini.seqexec.server

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZoneId}

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.seqexec.server.DhsClient.KeywordBag
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.dataflow.GsaAspect.Visibility
import edu.gemini.spModel.dataflow.GsaSequenceEditor.{HEADER_VISIBILITY_KEY, PROPRIETARY_MONTHS_KEY}
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants._

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

/**
  * Created by jluhrs on 1/31/17.
  */


trait ObsKeywordsReader {
  def getObsType: SeqAction[String]
  def getObsClass: SeqAction[String]
  def getGemPrgId: SeqAction[String]
  def getObsId: SeqAction[String]
  def getObservatory: SeqAction[String]
  def getTelescope: SeqAction[String]
  def getPwfs1Guide: SeqAction[StandardGuideOptions.Value]
  def getPwfs2Guide: SeqAction[StandardGuideOptions.Value]
  def getOiwfsGuide: SeqAction[StandardGuideOptions.Value]
  def getAowfsGuide: SeqAction[StandardGuideOptions.Value]
  def getHeaderPrivacy: SeqAction[Boolean]
  def getProprietaryMonths: SeqAction[String]
}

case class ObsKeywordReaderImpl(config: Config, telescope: String) extends ObsKeywordsReader {
  override def getObsType: SeqAction[String] = SeqAction(
    config.getItemValue(new ItemKey(OBSERVE_KEY, OBSERVE_TYPE_PROP)).toString)

  override def getObsClass: SeqAction[String] = SeqAction(
    config.getItemValue(new ItemKey(OBSERVE_KEY, OBS_CLASS_PROP)).toString)

  override def getGemPrgId: SeqAction[String] = SeqAction(
    config.getItemValue(new ItemKey(OCS_KEY, PROGRAMID_PROP)).toString)

  override def getObsId: SeqAction[String] = {
    val v = config.getItemValue(new ItemKey(OCS_KEY, OBSERVATIONID_PROP)).toString.split("-").toList.lastOption.getOrElse("")

    SeqAction(v)
  }

  override def getObservatory: SeqAction[String] = SeqAction(telescope)

  override def getTelescope: SeqAction[String] = SeqAction(telescope)

  override def getPwfs1Guide: SeqAction[StandardGuideOptions.Value] = EitherT(Task.now(
    config.extract(new ItemKey(TELESCOPE_KEY, Tcs.GUIDE_WITH_PWFS1_PROP)).as[StandardGuideOptions.Value].leftMap(e =>
      SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
  ))

  override def getPwfs2Guide: SeqAction[StandardGuideOptions.Value] = EitherT(Task.now(
    config.extract(new ItemKey(TELESCOPE_KEY, Tcs.GUIDE_WITH_PWFS2_PROP)).as[StandardGuideOptions.Value].leftMap(e =>
          SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
  ))

  override def getOiwfsGuide: SeqAction[StandardGuideOptions.Value] = EitherT(Task.now(
    config.extract(new ItemKey(TELESCOPE_KEY, GUIDE_WITH_OIWFS_PROP)).as[StandardGuideOptions.Value].leftMap(e =>
          SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
  ))

  override def getAowfsGuide: SeqAction[StandardGuideOptions.Value] = EitherT(Task.now(
    config.extract(new ItemKey(TELESCOPE_KEY, Tcs.GUIDE_WITH_AOWFS_PROP)).as[StandardGuideOptions.Value]
      .recoverWith[ConfigUtilOps.ExtractFailure, StandardGuideOptions.Value] {
        case ConfigUtilOps.KeyNotFound(_)         => StandardGuideOptions.Value.park.right
        case e@ConfigUtilOps.ConversionError(_,_) => e.left
      }.leftMap(e =>SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
  ))

  private val headerPrivacy: Boolean = config.extract(HEADER_VISIBILITY_KEY).as[Visibility].getOrElse(Visibility.PUBLIC) match {
    case Visibility.PRIVATE => true
    case _                  => false
  }

  override def getHeaderPrivacy: SeqAction[Boolean] = SeqAction(headerPrivacy)

  override def getProprietaryMonths: SeqAction[String] =
    if(headerPrivacy) {
      EitherT(Task.delay(
        config.extract(PROPRIETARY_MONTHS_KEY).as[Integer].recoverWith[ConfigUtilOps.ExtractFailure, Integer]{
          case ConfigUtilOps.KeyNotFound(_) => (new Integer(0)).right
          case e@ConfigUtilOps.ConversionError(_, _) => e.left
        }.leftMap(e =>SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
          .map(v => (LocalDate.now(ZoneId.of("GMT")).plusMonths(v.toLong).format(DateTimeFormatter.ISO_LOCAL_DATE)))))
    }
    else SeqAction(LocalDate.now(ZoneId.of("GMT")).format(DateTimeFormatter.ISO_LOCAL_DATE))
}

class StandardHeader(hs: DhsClient, obsReader: ObsKeywordsReader, tcsReader: TcsKeywordsReader) extends Header {
  import Header._
  override def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit] = {

    val p: SeqAction[Double] = for {
      xoff <- tcsReader.getXOffset
      yoff <- tcsReader.getYOffset
      iaa <- tcsReader.getInstrumentAA
    } yield -xoff * Math.cos(Math.toRadians(iaa)) + yoff * Math.sin(Math.toRadians(iaa))

    val q: SeqAction[Double] = for {
      xoff <- tcsReader.getXOffset
      yoff <- tcsReader.getYOffset
      iaa <- tcsReader.getInstrumentAA
    } yield -xoff * Math.sin(Math.toRadians(iaa)) - yoff * Math.cos(Math.toRadians(iaa))

    val raoff: SeqAction[Double] = for {
      poff <- p
      qoff <- q
      ipa  <- tcsReader.getInstrumentPA
    } yield poff * Math.cos(Math.toRadians(ipa)) + qoff * Math.sin(Math.toRadians(ipa))

    val decoff: SeqAction[Double] = for {
      poff <- p
      qoff <- q
      ipa  <- tcsReader.getInstrumentPA
    } yield poff * Math.cos(Math.toRadians(ipa)) + qoff * Math.sin(Math.toRadians(ipa))

    def guiderKeywords(guideWith: SeqAction[StandardGuideOptions.Value], baseName: String, target: TargetKeywordsReader,
                      extras: List[KeywordBag => SeqAction[KeywordBag]]): SeqAction[Unit] = guideWith.flatMap { g =>
      if (g == StandardGuideOptions.Value.guide) sendKeywords(id, inst, hs, List(
        buildDouble(target.getRA, baseName + "ARA"),
        buildDouble(target.getDec, baseName + "ADEC"),
        buildDouble(target.getRadialVelocity, baseName + "ARV"),
        buildDouble(target.getWavelength, baseName + "AWAVEL"),
        buildDouble(target.getEpoch, baseName + "AEPOCH"),
        buildDouble(target.getEquinox, baseName + "AEQUIN"),
        buildString(target.getFrame, baseName + "AFRAME"),
        buildString(target.getObjectName, baseName + "AOBJEC"),
        buildDouble(target.getProperMotionDec, baseName + "APMDEC"),
        buildDouble(target.getProperMotionRA, baseName + "APMRA"),
        buildDouble(target.getParallax, baseName + "APARAL")
      ) ++ extras)
      else SeqAction(List())
    }

    def standardGuiderKeywords(guideWith: SeqAction[StandardGuideOptions.Value], baseName: String,
                               target: TargetKeywordsReader, extras: List[KeywordBag => SeqAction[KeywordBag]]): SeqAction[Unit] =
      guiderKeywords(guideWith, baseName, target, List(buildDouble(tcsReader.getM2UserFocusOffset, baseName + "FOCUS")) ++ extras)

    val pwfs1Keywords = standardGuiderKeywords(obsReader.getPwfs1Guide, "P1", tcsReader.getPwfs1Target,
          List(buildDouble(tcsReader.getPwfs1Freq, "P1FREQ")))

    val pwfs2Keywords = standardGuiderKeywords(obsReader.getPwfs2Guide, "P2", tcsReader.getPwfs2Target,
          List(buildDouble(tcsReader.getPwfs2Freq, "P2FREQ")))

    val aowfsKeywords = standardGuiderKeywords(obsReader.getAowfsGuide, "AO", tcsReader.getAowfsTarget, List())

    val oiwfsKeywords = guiderKeywords(obsReader.getOiwfsGuide, "OI", tcsReader.getOiwfsTarget,
      List(buildDouble(tcsReader.getOiwfsFreq, "OIFREQ")))


    sendKeywords(id, inst, hs, List(
      buildString(obsReader.getObsType, "OBSTYPE"),
      buildString(obsReader.getObsClass, "OBSCLASS"),
      buildString(obsReader.getGemPrgId, "GEMPRGID"),
      buildString(obsReader.getObsId, "obsid"),
      buildString(obsReader.getObservatory, "OBSERVAT"),
      buildString(obsReader.getTelescope, "telescope"),
      buildBoolean(obsReader.getHeaderPrivacy, "PROP_MD"),
      buildString(obsReader.getProprietaryMonths, "RELEASE"),
      buildString(tcsReader.getHourAngle, "HA"),
      buildString(tcsReader.getLocalTime, "LT"),
      buildString(tcsReader.getTrackingFrame, "TRKFRAME"),
      buildDouble(tcsReader.getTrackingDec, "DECTRACK"),
      buildDouble(tcsReader.getTrackingRA, "RATRACK"),
      buildDouble(tcsReader.getTrackingEpoch, "TRKEPOCH"),
      buildString(tcsReader.getSourceATarget.getFrame, "FRAME"),
      buildDouble(tcsReader.getSourceATarget.getProperMotionDec, "PMDEC"),
      buildDouble(tcsReader.getSourceATarget.getProperMotionRA, "PMRA"),
      buildDouble(tcsReader.getSourceATarget.getWavelength, "WAVELENG"),
      buildDouble(tcsReader.getSourceATarget.getParallax, "PARALLAX"),
      buildDouble(tcsReader.getSourceATarget.getRadialVelocity, "RADVEL"),
      buildDouble(tcsReader.getSourceATarget.getEpoch, "EPOCH"),
      buildDouble(tcsReader.getSourceATarget.getEquinox, "EQUINOX"),
      buildDouble(tcsReader.getSourceATarget.getRA, "RA"),
      buildDouble(tcsReader.getSourceATarget.getDec, "DEC"),
      buildDouble(tcsReader.getTrackingEquinox, "TRKEQUIN"),
      buildDouble(tcsReader.getElevation, "ELEVATIO"),
      buildDouble(tcsReader.getAzimuth, "AZIMUTH"),
      buildDouble(tcsReader.getCRPositionAngle, "CRPA"),
      buildString(tcsReader.getUT, "UT"),
      buildString(tcsReader.getDate, "DATE"),
      buildString(tcsReader.getM2Baffle, "M2BAFFLE"),
      buildString(tcsReader.getM2CentralBaffle, "M2CENBAF"),
      buildString(tcsReader.getST, "ST"),
      buildDouble(tcsReader.getSFRotation, "SFRT2"),
      buildDouble(tcsReader.getSFTilt, "SFTILT"),
      buildDouble(tcsReader.getSFLinear, "SFLINEAR"),
      buildDouble(tcsReader.getInstrumentPA, "PA"),
      buildDouble(tcsReader.getInstrumentAA, "IAA"),
      buildDouble(tcsReader.getXOffset, "XOFFSET"),
      buildDouble(tcsReader.getYOffset, "YOFFSET"),
      buildDouble(p, "POFFSET"),
      buildDouble(q, "QOFFSET"),
      buildDouble(raoff, "RAOFFSET"),
      buildDouble(decoff, "DECOFFSE"),
      buildString(tcsReader.getAOFoldName, "AOFOLD"),
      buildString(obsReader.getPwfs1Guide.map(_.toString), "PWFS1_ST"),
      buildString(obsReader.getPwfs2Guide.map(_.toString), "PWFS2_ST"),
      buildString(obsReader.getOiwfsGuide.map(_.toString), "OIWFS_ST"),
      buildString(obsReader.getAowfsGuide.map(_.toString), "AOWFS_ST")
    )) *>
    pwfs1Keywords *>
    pwfs2Keywords *>
    oiwfsKeywords *>
    aowfsKeywords
  }



  override def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit] = sendKeywords(id, inst, hs,
    List(
      buildDouble(tcsReader.getAirMass, "AIRMASS"),
      buildDouble(tcsReader.getStartAirMass, "AMSTART"),
      buildDouble(tcsReader.getEndAirMass, "AMEND")
    ))
}
