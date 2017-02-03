package edu.gemini.seqexec.server

import edu.gemini.seqexec.model.dhs.ObsId
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.seqcomp.SeqConfigNames._

/**
  * Created by jluhrs on 1/31/17.
  */

trait TcsKeywordsReader {
  def getHourAngle: SeqAction[String]
  def getLocalTime: SeqAction[String]
  def getTrackingFrame: SeqAction[String]
  def getTrackingEpoch(): SeqAction[Double]
  def getTrackingEquinox: SeqAction[Double]
  def getTrackingDec: SeqAction[Double]
  def getTrackingRA: SeqAction[Double]
  def getFrame: SeqAction[String]
  def getEpoch: SeqAction[Double]
  def getProperMotionDec: SeqAction[Double]
  def getProperMotionRA: SeqAction[Double]
  def getWavelength: SeqAction[Double]
  def getParallax: SeqAction[Double]
  def getRadialVelocity: SeqAction[Double]
  def getEquinox: SeqAction[Double]
  def getRA: SeqAction[Double]
  def getDec: SeqAction[Double]
  def getElevation: SeqAction[Double]
  def getAzimuth: SeqAction[Double]
  def getCRPositionAngle: SeqAction[Double]
  def getUT: SeqAction[String]
}

object DummyTcsKeywordsReader extends TcsKeywordsReader {
  override def getHourAngle: SeqAction[String] = SeqAction("00:00:00")
  override def getLocalTime: SeqAction[String] = SeqAction("00:00:00")
  override def getTrackingFrame: SeqAction[String] = SeqAction("FK5")
  override def getTrackingEpoch(): SeqAction[Double] = SeqAction(0.0)
  override def getTrackingEquinox: SeqAction[Double] = SeqAction(2000.0)
  override def getTrackingDec: SeqAction[Double] = SeqAction(0.0)
  override def getTrackingRA: SeqAction[Double] = SeqAction(0.0)
  override def getFrame: SeqAction[String] = SeqAction("FK5")
  override def getEpoch: SeqAction[Double] = SeqAction(0.0)
  override def getProperMotionDec: SeqAction[Double] = SeqAction(0.0)
  override def getProperMotionRA: SeqAction[Double] = SeqAction(0.0)
  override def getWavelength: SeqAction[Double] = SeqAction(0.0)
  override def getParallax: SeqAction[Double] = SeqAction(0.0)
  override def getRadialVelocity: SeqAction[Double] = SeqAction(0.0)
  override def getEquinox: SeqAction[Double] = SeqAction(2000.0)
  override def getRA: SeqAction[Double] = SeqAction(0.0)
  override def getDec: SeqAction[Double] = SeqAction(0.0)
  override def getElevation: SeqAction[Double] = SeqAction(0.0)
  override def getAzimuth: SeqAction[Double] = SeqAction(0.0)
  override def getCRPositionAngle: SeqAction[Double] = SeqAction(0.0)
  override def getUT: SeqAction[String] = SeqAction("00:00:00")
}

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
  override def sendBefore(id: ObsId, inst: String): SeqAction[Unit] = sendKeywords(id, inst, hs, List(
    buildString(obsReader.getObsType, "OBSTYPE"),
    buildString(obsReader.getObsClass, "OBSCLASS"),
    buildString(obsReader.getGemPrgId, "GEMPRGID"),
    buildString(obsReader.getObsId, "obsid"),
    buildString(obsReader.getObservatory, "OBSERVAT"),
    buildString(obsReader.getTelescope, "telescope")
  ))

  override def sendAfter(id: ObsId, inst: String): SeqAction[Unit] = SeqAction(())
}
