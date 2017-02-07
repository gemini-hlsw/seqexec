package edu.gemini.seqexec.server

import java.time.LocalDate
import java.time.format.DateTimeFormatter

/**
  * Created by jluhrs on 2/6/17.
  */
trait TargetKeywordsReader {
  def getRA: SeqAction[Double]
  def getDec: SeqAction[Double]
  def getRadialVelocity: SeqAction[Double]
  def getParallax: SeqAction[Double]
  def getWavelength: SeqAction[Double]
  def getEpoch: SeqAction[Double]
  def getEquinox: SeqAction[Double]
  def getFrame: SeqAction[String]
  def getObjectName: SeqAction[String]
  def getProperMotionDec: SeqAction[Double]
  def getProperMotionRA: SeqAction[Double]
}

trait TcsKeywordsReader {
  def getHourAngle: SeqAction[String]
  def getLocalTime: SeqAction[String]
  def getTrackingFrame: SeqAction[String]
  def getTrackingEpoch: SeqAction[Double]
  def getTrackingEquinox: SeqAction[Double]
  def getTrackingDec: SeqAction[Double]
  def getTrackingRA: SeqAction[Double]
  def getElevation: SeqAction[Double]
  def getAzimuth: SeqAction[Double]
  def getCRPositionAngle: SeqAction[Double]
  def getUT: SeqAction[String]
  def getDate: SeqAction[String]
  def getM2Baffle: SeqAction[String]
  def getM2CentralBaffle: SeqAction[String]
  def getST: SeqAction[String]
  def getSFRotation: SeqAction[Double]
  def getSFTilt: SeqAction[Double]
  def getSFLinear: SeqAction[Double]
  def getInstrumentPA: SeqAction[Double]
  def getTarget: SeqAction[String]
  def getXOffset: SeqAction[Double]
  def getYOffset: SeqAction[Double]
  def getInstrumentAA: SeqAction[Double]
  def getAOFoldName: SeqAction[String]
  def getSourceATarget: TargetKeywordsReader
  def getAirMass: SeqAction[Double]
  def getStartAirMass: SeqAction[Double]
  def getEndAirMass: SeqAction[Double]

}

object DummyTargetKeywordsReader extends TargetKeywordsReader {
  override def getRA: SeqAction[Double] = SeqAction(0.0)
  override def getDec: SeqAction[Double] = SeqAction(0.0)
  override def getRadialVelocity: SeqAction[Double] = SeqAction(0.0)
  override def getParallax: SeqAction[Double] = SeqAction(0.0)
  override def getWavelength: SeqAction[Double] = SeqAction(0.0)
  override def getEpoch: SeqAction[Double] = SeqAction(0.0)
  override def getEquinox: SeqAction[Double] = SeqAction(0.0)
  override def getFrame: SeqAction[String] = SeqAction("FK5")
  override def getObjectName: SeqAction[String] = SeqAction("Dummy")
  override def getProperMotionDec: SeqAction[Double] = SeqAction(0.0)
  override def getProperMotionRA: SeqAction[Double] = SeqAction(0.0)
}

object DummyTcsKeywordsReader extends TcsKeywordsReader {
  override def getHourAngle: SeqAction[String] = SeqAction("00:00:00")
  override def getLocalTime: SeqAction[String] = SeqAction("00:00:00")
  override def getTrackingFrame: SeqAction[String] = SeqAction("FK5")
  override def getTrackingEpoch: SeqAction[Double] = SeqAction(0.0)
  override def getTrackingEquinox: SeqAction[Double] = SeqAction(2000.0)
  override def getTrackingDec: SeqAction[Double] = SeqAction(0.0)
  override def getTrackingRA: SeqAction[Double] = SeqAction(0.0)
  override def getElevation: SeqAction[Double] = SeqAction(0.0)
  override def getAzimuth: SeqAction[Double] = SeqAction(0.0)
  override def getCRPositionAngle: SeqAction[Double] = SeqAction(0.0)
  override def getUT: SeqAction[String] = SeqAction("00:00:00")
  override def getDate: SeqAction[String] = SeqAction(LocalDate.now.format(DateTimeFormatter.ISO_LOCAL_DATE))
  override def getM2Baffle: SeqAction[String] = SeqAction("OUT")
  override def getM2CentralBaffle: SeqAction[String] = SeqAction("OUT")
  override def getST: SeqAction[String] = SeqAction("00:00:00")
  override def getSFRotation: SeqAction[Double] = SeqAction(0.0)
  override def getSFTilt: SeqAction[Double] = SeqAction(0.0)
  override def getSFLinear: SeqAction[Double] = SeqAction(0.0)
  override def getInstrumentPA: SeqAction[Double] = SeqAction(0.0)
  override def getTarget: SeqAction[String] = SeqAction("Dummy")
  override def getXOffset: SeqAction[Double] = SeqAction(0.0)
  override def getYOffset: SeqAction[Double] = SeqAction(0.0)
  override def getInstrumentAA: SeqAction[Double] = SeqAction(0.0)
  override def getAOFoldName: SeqAction[String] = SeqAction("OUT")
  override def getSourceATarget: TargetKeywordsReader = DummyTargetKeywordsReader
  override def getAirMass: SeqAction[Double] = SeqAction(1.0)
  override def getStartAirMass: SeqAction[Double] = SeqAction(1.0)
  override def getEndAirMass: SeqAction[Double] = SeqAction(1.0)
}
