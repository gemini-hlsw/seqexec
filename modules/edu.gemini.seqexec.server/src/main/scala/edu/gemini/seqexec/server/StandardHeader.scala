package edu.gemini.seqexec.server

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import edu.gemini.seqexec.model.dhs.ObsId
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.seqcomp.SeqConfigNames._

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
}

object DummyObsKeywordsReader extends ObsKeywordsReader {
  override def getObsType: SeqAction[String] = SeqAction("Science")
  override def getObsClass: SeqAction[String] = SeqAction("observe")
  override def getGemPrgId: SeqAction[String] = SeqAction("GS-ENG20170201")
  override def getObsId: SeqAction[String] = SeqAction("GS-ENG20170201-1")
  override def getObservatory: SeqAction[String] = SeqAction("Gemini-South")
  override def getTelescope: SeqAction[String] = SeqAction("Gemini-South")
}


class StandardHeader(hs: DhsClient, obsReader: ObsKeywordsReader, tcsReader: TcsKeywordsReader) extends Header {
  import Header._
  override def sendBefore(id: ObsId, inst: String): SeqAction[Unit] = {

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

    sendKeywords(id, inst, hs, List(
      buildString(obsReader.getObsType, "OBSTYPE"),
      buildString(obsReader.getObsClass, "OBSCLASS"),
      buildString(obsReader.getGemPrgId, "GEMPRGID"),
      buildString(obsReader.getObsId, "obsid"),
      buildString(obsReader.getObservatory, "OBSERVAT"),
      buildString(obsReader.getTelescope, "telescope"),
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
      buildString(tcsReader.getAOFoldName, "AOFOLD")

    ))
  }



  override def sendAfter(id: ObsId, inst: String): SeqAction[Unit] = sendKeywords(id, inst, hs,
    List(
      buildDouble(tcsReader.getAirMass, "AIRMASS"),
      buildDouble(tcsReader.getStartAirMass, "AMSTART"),
      buildDouble(tcsReader.getEndAirMass, "AMEND")
    ))
}
