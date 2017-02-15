package edu.gemini.seqexec.server

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import scalaz._
import Scalaz._
import scala.util.Try

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
  def getXOffset: SeqAction[Double]
  def getYOffset: SeqAction[Double]
  def getTrackingRAOffset: SeqAction[Double]
  def getTrackingDecOffset: SeqAction[Double]
  def getInstrumentAA: SeqAction[Double]
  def getAOFoldName: SeqAction[String]
  def getSourceATarget: TargetKeywordsReader
  def getPwfs1Target: TargetKeywordsReader
  def getPwfs2Target: TargetKeywordsReader
  def getOiwfsTarget: TargetKeywordsReader
  def getAowfsTarget: TargetKeywordsReader
  def getGwfs1Target: TargetKeywordsReader
  def getGwfs2Target: TargetKeywordsReader
  def getGwfs3Target: TargetKeywordsReader
  def getGwfs4Target: TargetKeywordsReader
  def getAirMass: SeqAction[Double]
  def getStartAirMass: SeqAction[Double]
  def getEndAirMass: SeqAction[Double]
  def getM2UserFocusOffset: SeqAction[Double]
  def getPwfs1Freq: SeqAction[Double]
  def getPwfs2Freq: SeqAction[Double]
  def getOiwfsFreq: SeqAction[Double]
}

object DummyTargetKeywordsReader extends TargetKeywordsReader {
  override def getRA: SeqAction[Double] = SeqAction(0.0)
  override def getDec: SeqAction[Double] = SeqAction(0.0)
  override def getRadialVelocity: SeqAction[Double] = SeqAction(0.0)
  override def getParallax: SeqAction[Double] = SeqAction(0.0)
  override def getWavelength: SeqAction[Double] = SeqAction(0.0)
  override def getEpoch: SeqAction[Double] = SeqAction(2000.0)
  override def getEquinox: SeqAction[Double] = SeqAction(2000.0)
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
  override def getXOffset: SeqAction[Double] = SeqAction(0.0)
  override def getYOffset: SeqAction[Double] = SeqAction(0.0)
  override def getInstrumentAA: SeqAction[Double] = SeqAction(0.0)
  override def getAOFoldName: SeqAction[String] = SeqAction("OUT")
  override def getSourceATarget: TargetKeywordsReader = DummyTargetKeywordsReader
  override def getAirMass: SeqAction[Double] = SeqAction(1.0)
  override def getStartAirMass: SeqAction[Double] = SeqAction(1.0)
  override def getEndAirMass: SeqAction[Double] = SeqAction(1.0)

  override def getTrackingRAOffset: SeqAction[Double] = SeqAction(0.0)

  override def getTrackingDecOffset: SeqAction[Double] = SeqAction(0.0)

  override def getPwfs1Target: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getPwfs2Target: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getOiwfsTarget: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getAowfsTarget: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getGwfs1Target: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getGwfs2Target: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getGwfs3Target: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getGwfs4Target: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getM2UserFocusOffset: SeqAction[Double] = SeqAction(0.0)

  override def getPwfs1Freq: SeqAction[Double] = SeqAction(-9999.0)

  override def getPwfs2Freq: SeqAction[Double] = SeqAction(-9999.0)

  override def getOiwfsFreq: SeqAction[Double] = SeqAction(-9999.0)
}

object TcsKeywordsReaderImpl extends TcsKeywordsReader {

  implicit def fromStringOption(v: Option[String]): SeqAction[String] = SeqAction(v.getOrElse("No Value"))
  implicit def fromDoubleOption(v: Option[Double]): SeqAction[Double] = SeqAction(v.getOrElse(9999.0))
  implicit def fromIntOption(v: Option[Int]): SeqAction[Int] = SeqAction(v.getOrElse(9999))
  implicit def fromBooleanOption(v: Option[Boolean]): SeqAction[Boolean] = SeqAction(v.getOrElse(false))

  override def getHourAngle: SeqAction[String] = TcsEpics.instance.hourAngle
  override def getLocalTime: SeqAction[String] = TcsEpics.instance.localTime
  override def getTrackingFrame: SeqAction[String] = TcsEpics.instance.trackingFrame
  override def getTrackingEpoch: SeqAction[Double] = TcsEpics.instance.trackingEpoch

  def translateEpoch(v: Option[String]): Option[Double] = v.flatMap(x => Try { x.drop(1).toDouble }.toOption)

  override def getTrackingEquinox: SeqAction[Double] = translateEpoch(TcsEpics.instance.trackingEquinox)
  override def getTrackingDec: SeqAction[Double] = TcsEpics.instance.trackingDec
  override def getTrackingRA: SeqAction[Double] = TcsEpics.instance.trackingRA
  override def getElevation: SeqAction[Double] = TcsEpics.instance.elevation
  override def getAzimuth: SeqAction[Double] = TcsEpics.instance.azimuth
  override def getCRPositionAngle: SeqAction[Double] = TcsEpics.instance.crPositionAngle
  override def getUT: SeqAction[String] = TcsEpics.instance.ut
  override def getDate: SeqAction[String] = TcsEpics.instance.date
  override def getM2Baffle: SeqAction[String] = TcsEpics.instance.m2Baffle
  override def getM2CentralBaffle: SeqAction[String] = TcsEpics.instance.m2CentralBaffle
  override def getST: SeqAction[String] = TcsEpics.instance.st
  override def getSFRotation: SeqAction[Double] = TcsEpics.instance.sfRotation
  override def getSFTilt: SeqAction[Double] = TcsEpics.instance.sfTilt
  override def getSFLinear: SeqAction[Double] = TcsEpics.instance.sfLinear
  override def getInstrumentPA: SeqAction[Double] = TcsEpics.instance.instrPA

  private val xoffIndex = 6
  private val yoffIndex = 7
  private val focalPlaneScale = 1.611443804

  override def getXOffset: SeqAction[Double] = TcsEpics.instance.targetA.flatMap(v => v.lift(xoffIndex).map(_*focalPlaneScale))
  override def getYOffset: SeqAction[Double] = TcsEpics.instance.targetA.flatMap(v => v.lift(yoffIndex).map(_*focalPlaneScale))

  private val decIndex = 1
  private val raoffIndex = 2
  private val decoffIndex = 3
  private val degreeToArcsec = 3600.0

  override def getTrackingRAOffset: SeqAction[Double] = {
    def angleRange(v: Double) = {
      val r = v % 360.0
      if (r<180.0) r + 360.0
      else if (r>=180.0) r - 360.0
      else r
    }
    def raOffset(off: Double, dec: Double): Double = angleRange(Math.toDegrees(off))*Math.cos(dec)*degreeToArcsec

    TcsEpics.instance.targetA.flatMap(v => Apply[Option].lift2(raOffset)(v.lift(raoffIndex),v.lift(decoffIndex)))
  }

  override def getTrackingDecOffset: SeqAction[Double] =
    TcsEpics.instance.targetA.flatMap(v => v.lift(decoffIndex).map(Math.toDegrees(_)*degreeToArcsec))
  override def getInstrumentAA: SeqAction[Double] = TcsEpics.instance.instrAA
  override def getAOFoldName: SeqAction[String] = TcsEpics.instance.aoFoldPosition

  private def target(t: TcsEpics.Target) = new TargetKeywordsReader {
    override def getRA: SeqAction[Double] = t.ra
    override def getDec: SeqAction[Double] = t.dec
    override def getRadialVelocity: SeqAction[Double] = t.radialVelocity
    override def getParallax: SeqAction[Double] = t.parallax
    override def getWavelength: SeqAction[Double] = t.centralWavelenght
    override def getEpoch: SeqAction[Double] = translateEpoch(t.epoch)
    override def getEquinox: SeqAction[Double] = translateEpoch(t.equinox)
    override def getFrame: SeqAction[String] = t.frame
    override def getObjectName: SeqAction[String] = t.objectName
    override def getProperMotionDec: SeqAction[Double] = t.properMotionDec
    override def getProperMotionRA: SeqAction[Double] = t.properMotionRA
  }

  override def getSourceATarget: TargetKeywordsReader = target(TcsEpics.instance.sourceATarget)
  override def getAirMass: SeqAction[Double] =  TcsEpics.instance.airmass
  override def getStartAirMass: SeqAction[Double] = TcsEpics.instance.airmassStart
  override def getEndAirMass: SeqAction[Double] = TcsEpics.instance.airmassEnd

  override def getPwfs1Target: TargetKeywordsReader = target(TcsEpics.instance.pwfs1Target)

  override def getPwfs2Target: TargetKeywordsReader = target(TcsEpics.instance.pwfs2Target)

  override def getOiwfsTarget: TargetKeywordsReader = target(TcsEpics.instance.oiwfsTarget)

  override def getAowfsTarget: TargetKeywordsReader = target(TcsEpics.instance.pwfs2Target)

  override def getGwfs1Target: TargetKeywordsReader = target(TcsEpics.instance.gwfs1Target)

  override def getGwfs2Target: TargetKeywordsReader = target(TcsEpics.instance.gwfs2Target)

  override def getGwfs3Target: TargetKeywordsReader = target(TcsEpics.instance.gwfs3Target)

  override def getGwfs4Target: TargetKeywordsReader = target(TcsEpics.instance.gwfs4Target)

  override def getM2UserFocusOffset: SeqAction[Double] = TcsEpics.instance.m2UserFocusOffset

  private def calcFrequency(t: Double): Double = if (t > 0.0) 1.0 / t else -9999.0

  override def getPwfs1Freq: SeqAction[Double] = TcsEpics.instance.pwfs1IntegrationTime.map(calcFrequency)

  override def getPwfs2Freq: SeqAction[Double] = TcsEpics.instance.pwfs2IntegrationTime.map(calcFrequency)

  override def getOiwfsFreq: SeqAction[Double] = TcsEpics.instance.oiwfsIntegrationTime.map(calcFrequency)
}