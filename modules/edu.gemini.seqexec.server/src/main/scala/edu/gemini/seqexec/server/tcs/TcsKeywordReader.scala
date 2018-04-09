// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.tcs

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.Apply
import cats.implicits._
import edu.gemini.seqexec.server.SeqAction
import edu.gemini.spModel.core.Wavelength
import squants.space.{Angstroms, Meters}

import scala.util.Try

trait TargetKeywordsReader {
  def getRA: SeqAction[Option[Double]]
  def getDec: SeqAction[Option[Double]]
  def getRadialVelocity: SeqAction[Option[Double]]
  def getParallax: SeqAction[Option[Double]]
  def getWavelength: SeqAction[Option[Wavelength]]
  def getEpoch: SeqAction[Option[Double]]
  def getEquinox: SeqAction[Option[Double]]
  def getFrame: SeqAction[Option[String]]
  def getObjectName: SeqAction[Option[String]]
  def getProperMotionDec: SeqAction[Option[Double]]
  def getProperMotionRA: SeqAction[Option[Double]]
}

trait TcsKeywordsReader {
  def getHourAngle: SeqAction[Option[String]]
  def getLocalTime: SeqAction[Option[String]]
  def getTrackingFrame: SeqAction[Option[String]]
  def getTrackingEpoch: SeqAction[Option[Double]]
  def getTrackingEquinox: SeqAction[Option[Double]]
  def getTrackingDec: SeqAction[Option[Double]]
  def getTrackingRA: SeqAction[Option[Double]]
  def getElevation: SeqAction[Option[Double]]
  def getAzimuth: SeqAction[Option[Double]]
  def getCRPositionAngle: SeqAction[Option[Double]]
  def getUT: SeqAction[Option[String]]
  def getDate: SeqAction[Option[String]]
  def getM2Baffle: SeqAction[Option[String]]
  def getM2CentralBaffle: SeqAction[Option[String]]
  def getST: SeqAction[Option[String]]
  def getSFRotation: SeqAction[Option[Double]]
  def getSFTilt: SeqAction[Option[Double]]
  def getSFLinear: SeqAction[Option[Double]]
  def getInstrumentPA: SeqAction[Option[Double]]
  def getXOffset: SeqAction[Option[Double]]
  def getYOffset: SeqAction[Option[Double]]
  def getTrackingRAOffset: SeqAction[Option[Double]]
  def getTrackingDecOffset: SeqAction[Option[Double]]
  def getInstrumentAA: SeqAction[Option[Double]]
  def getAOFoldName: SeqAction[Option[String]]
  def getSourceATarget: TargetKeywordsReader
  def getPwfs1Target: TargetKeywordsReader
  def getPwfs2Target: TargetKeywordsReader
  def getOiwfsTarget: TargetKeywordsReader
  def getAowfsTarget: TargetKeywordsReader
  def getGwfs1Target: TargetKeywordsReader
  def getGwfs2Target: TargetKeywordsReader
  def getGwfs3Target: TargetKeywordsReader
  def getGwfs4Target: TargetKeywordsReader
  def getAirMass: SeqAction[Option[Double]]
  def getStartAirMass: SeqAction[Option[Double]]
  def getEndAirMass: SeqAction[Option[Double]]
  def getCarouselMode: SeqAction[Option[String]]
  def getM2UserFocusOffset: SeqAction[Option[Double]]
  def getPwfs1Freq: SeqAction[Option[Double]]
  def getPwfs2Freq: SeqAction[Option[Double]]
  def getOiwfsFreq: SeqAction[Option[Double]]
  def getGmosInstPort: SeqAction[Option[Int]]
  def getGnirsInstPort: SeqAction[Option[Int]]
  def getGpiInstPort: SeqAction[Option[Int]]
  def getNiriInstPort: SeqAction[Option[Int]]
  def getNifsInstPort: SeqAction[Option[Int]]
  def getGsaoiInstPort: SeqAction[Option[Int]]
  def getF2InstPort: SeqAction[Option[Int]]
}

object DummyTargetKeywordsReader extends TargetKeywordsReader {
  import edu.gemini.seqexec.server.KeywordsReader._

  override def getRA: SeqAction[Option[Double]] = 0.0
  override def getDec: SeqAction[Option[Double]] = 0.0
  override def getRadialVelocity: SeqAction[Option[Double]] = 0.0
  override def getParallax: SeqAction[Option[Double]] = 0.0
  override def getWavelength: SeqAction[Option[Wavelength]] = Wavelength(Meters(0.0))
  override def getEpoch: SeqAction[Option[Double]] = 2000.0
  override def getEquinox: SeqAction[Option[Double]] = 2000.0
  override def getFrame: SeqAction[Option[String]] = "FK5"
  override def getObjectName: SeqAction[Option[String]] = "Dummy"
  override def getProperMotionDec: SeqAction[Option[Double]] = 0.0
  override def getProperMotionRA: SeqAction[Option[Double]] = 0.0
}

object DummyTcsKeywordsReader extends TcsKeywordsReader {
  import edu.gemini.seqexec.server.KeywordsReader._

  override def getHourAngle: SeqAction[Option[String]] = "00:00:00"
  override def getLocalTime: SeqAction[Option[String]] = "00:00:00"
  override def getTrackingFrame: SeqAction[Option[String]] = "FK5"
  override def getTrackingEpoch: SeqAction[Option[Double]] = 0.0
  override def getTrackingEquinox: SeqAction[Option[Double]] = 2000.0
  override def getTrackingDec: SeqAction[Option[Double]] = 0.0
  override def getTrackingRA: SeqAction[Option[Double]] = 0.0
  override def getElevation: SeqAction[Option[Double]] = 0.0
  override def getAzimuth: SeqAction[Option[Double]] = 0.0
  override def getCRPositionAngle: SeqAction[Option[Double]] = 0.0
  override def getUT: SeqAction[Option[String]] = "00:00:00"
  override def getDate: SeqAction[Option[String]] = LocalDate.now.format(DateTimeFormatter.ISO_LOCAL_DATE)
  override def getM2Baffle: SeqAction[Option[String]] = "OUT"
  override def getM2CentralBaffle: SeqAction[Option[String]] = "OUT"
  override def getST: SeqAction[Option[String]] = "00:00:00"
  override def getSFRotation: SeqAction[Option[Double]] = 0.0
  override def getSFTilt: SeqAction[Option[Double]] = 0.0
  override def getSFLinear: SeqAction[Option[Double]] = 0.0
  override def getInstrumentPA: SeqAction[Option[Double]] = 0.0
  override def getXOffset: SeqAction[Option[Double]] = 0.0
  override def getYOffset: SeqAction[Option[Double]] = 0.0
  override def getInstrumentAA: SeqAction[Option[Double]] = 0.0
  override def getAOFoldName: SeqAction[Option[String]] = "OUT"
  override def getSourceATarget: TargetKeywordsReader = DummyTargetKeywordsReader
  override def getAirMass: SeqAction[Option[Double]] = 1.0
  override def getStartAirMass: SeqAction[Option[Double]] = 1.0
  override def getEndAirMass: SeqAction[Option[Double]] = 1.0

  override def getTrackingRAOffset: SeqAction[Option[Double]] = 0.0

  override def getTrackingDecOffset: SeqAction[Option[Double]] = 0.0

  override def getPwfs1Target: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getPwfs2Target: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getOiwfsTarget: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getAowfsTarget: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getGwfs1Target: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getGwfs2Target: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getGwfs3Target: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getGwfs4Target: TargetKeywordsReader = DummyTargetKeywordsReader

  override def getM2UserFocusOffset: SeqAction[Option[Double]] = 0.0

  override def getPwfs1Freq: SeqAction[Option[Double]] = -9999.0

  override def getPwfs2Freq: SeqAction[Option[Double]] = -9999.0

  override def getOiwfsFreq: SeqAction[Option[Double]] = -9999.0

  override def getCarouselMode: SeqAction[Option[String]] = "Basic"

  override def getGmosInstPort: SeqAction[Option[Int]] = 0

  override def getGnirsInstPort: SeqAction[Option[Int]] = 0
  override def getGpiInstPort: SeqAction[Option[Int]] = 0
  override def getNiriInstPort: SeqAction[Option[Int]] = 0
  override def getNifsInstPort: SeqAction[Option[Int]] = 0
  override def getGsaoiInstPort: SeqAction[Option[Int]] = 0
  override def getF2InstPort: SeqAction[Option[Int]] = 0
}

object TcsKeywordsReaderImpl extends TcsKeywordsReader {
  import edu.gemini.seqexec.server.KeywordsReader._

  override def getHourAngle: SeqAction[Option[String]] = TcsEpics.instance.hourAngle
  override def getLocalTime: SeqAction[Option[String]] = TcsEpics.instance.localTime
  override def getTrackingFrame: SeqAction[Option[String]] = TcsEpics.instance.trackingFrame
  override def getTrackingEpoch: SeqAction[Option[Double]] = TcsEpics.instance.trackingEpoch

  def translateEpoch(v: Option[String]): Option[Double] = v.flatMap(x => Try { x.drop(1).toDouble }.toOption)

  override def getTrackingEquinox: SeqAction[Option[Double]] = translateEpoch(TcsEpics.instance.trackingEquinox)
  override def getTrackingDec: SeqAction[Option[Double]] = TcsEpics.instance.trackingDec
  override def getTrackingRA: SeqAction[Option[Double]] = TcsEpics.instance.trackingRA
  override def getElevation: SeqAction[Option[Double]] = TcsEpics.instance.elevation
  override def getAzimuth: SeqAction[Option[Double]] = TcsEpics.instance.azimuth
  override def getCRPositionAngle: SeqAction[Option[Double]] = TcsEpics.instance.crPositionAngle
  override def getUT: SeqAction[Option[String]] = TcsEpics.instance.ut
  override def getDate: SeqAction[Option[String]] = TcsEpics.instance.date
  override def getM2Baffle: SeqAction[Option[String]] = TcsEpics.instance.m2Baffle
  override def getM2CentralBaffle: SeqAction[Option[String]] = TcsEpics.instance.m2CentralBaffle
  override def getST: SeqAction[Option[String]] = TcsEpics.instance.st
  override def getSFRotation: SeqAction[Option[Double]] = TcsEpics.instance.sfRotation
  override def getSFTilt: SeqAction[Option[Double]] = TcsEpics.instance.sfTilt
  override def getSFLinear: SeqAction[Option[Double]] = TcsEpics.instance.sfLinear
  override def getInstrumentPA: SeqAction[Option[Double]] = TcsEpics.instance.instrPA
  override def getGmosInstPort: SeqAction[Option[Int]] = TcsEpics.instance.gmosPort

  private val xoffIndex = 6
  private val yoffIndex = 7
  private val focalPlaneScale = 1.611443804

  override def getXOffset: SeqAction[Option[Double]] = TcsEpics.instance.targetA.flatMap(v => v.lift(xoffIndex).map(_*focalPlaneScale))
  override def getYOffset: SeqAction[Option[Double]] = TcsEpics.instance.targetA.flatMap(v => v.lift(yoffIndex).map(_*focalPlaneScale))

  private val raoffIndex = 2
  private val decoffIndex = 3
  private val degreeToArcsec = 3600.0

  override def getTrackingRAOffset: SeqAction[Option[Double]] = {
    def angleRange(v: Double) = {
      val r = v % 360.0
      if (r<180.0) r + 360.0
      else if (r>=180.0) r - 360.0
      else r
    }
    def raOffset(off: Double, dec: Double): Double = angleRange(Math.toDegrees(off))*Math.cos(dec)*degreeToArcsec

    TcsEpics.instance.targetA.flatMap(v => Apply[Option].ap2(Option(raOffset _))(v.lift(raoffIndex),v.lift(decoffIndex)))
  }

  override def getTrackingDecOffset: SeqAction[Option[Double]] =
    TcsEpics.instance.targetA.flatMap(v => v.lift(decoffIndex).map(Math.toDegrees(_)*degreeToArcsec))
  override def getInstrumentAA: SeqAction[Option[Double]] = TcsEpics.instance.instrAA
  override def getAOFoldName: SeqAction[Option[String]] = TcsEpics.instance.aoFoldPosition

  private def target(t: TcsEpics.Target) = new TargetKeywordsReader {
    override def getRA: SeqAction[Option[Double]] = SeqAction(t.ra)
    override def getDec: SeqAction[Option[Double]] = t.dec
    override def getRadialVelocity: SeqAction[Option[Double]] = t.radialVelocity
    override def getParallax: SeqAction[Option[Double]] = t.parallax
    override def getWavelength: SeqAction[Option[Wavelength]] = t.centralWavelenght.map(v => Wavelength(Angstroms[Double](v)))
    override def getEpoch: SeqAction[Option[Double]] = translateEpoch(t.epoch)
    override def getEquinox: SeqAction[Option[Double]] = translateEpoch(t.equinox)
    override def getFrame: SeqAction[Option[String]] = t.frame
    override def getObjectName: SeqAction[Option[String]] = t.objectName
    override def getProperMotionDec: SeqAction[Option[Double]] = t.properMotionDec
    override def getProperMotionRA: SeqAction[Option[Double]] = t.properMotionRA
  }

  override def getSourceATarget: TargetKeywordsReader = target(TcsEpics.instance.sourceATarget)
  override def getAirMass: SeqAction[Option[Double]] =  TcsEpics.instance.airmass
  override def getStartAirMass: SeqAction[Option[Double]] = TcsEpics.instance.airmassStart
  override def getEndAirMass: SeqAction[Option[Double]] = TcsEpics.instance.airmassEnd

  override def getPwfs1Target: TargetKeywordsReader = target(TcsEpics.instance.pwfs1Target)

  override def getPwfs2Target: TargetKeywordsReader = target(TcsEpics.instance.pwfs2Target)

  override def getOiwfsTarget: TargetKeywordsReader = target(TcsEpics.instance.oiwfsTarget)

  override def getAowfsTarget: TargetKeywordsReader = target(TcsEpics.instance.pwfs2Target)

  override def getGwfs1Target: TargetKeywordsReader = target(TcsEpics.instance.gwfs1Target)

  override def getGwfs2Target: TargetKeywordsReader = target(TcsEpics.instance.gwfs2Target)

  override def getGwfs3Target: TargetKeywordsReader = target(TcsEpics.instance.gwfs3Target)

  override def getGwfs4Target: TargetKeywordsReader = target(TcsEpics.instance.gwfs4Target)

  override def getM2UserFocusOffset: SeqAction[Option[Double]] = TcsEpics.instance.m2UserFocusOffset

  private def calcFrequency(t: Double): Double = if (t > 0.0) 1.0 / t else -9999.0

  override def getPwfs1Freq: SeqAction[Option[Double]] = TcsEpics.instance.pwfs1IntegrationTime.map(calcFrequency)

  override def getPwfs2Freq: SeqAction[Option[Double]] = TcsEpics.instance.pwfs2IntegrationTime.map(calcFrequency)

  override def getOiwfsFreq: SeqAction[Option[Double]] = TcsEpics.instance.oiwfsIntegrationTime.map(calcFrequency)

  override def getCarouselMode: SeqAction[Option[String]] = TcsEpics.instance.carouselMode

  override def getGnirsInstPort: SeqAction[Option[Int]] = TcsEpics.instance.gnirsPort

  override def getGpiInstPort: SeqAction[Option[Int]] = TcsEpics.instance.gpiPort

  override def getNiriInstPort: SeqAction[Option[Int]] = TcsEpics.instance.niriPort

  override def getNifsInstPort: SeqAction[Option[Int]] = TcsEpics.instance.nifsPort

  override def getGsaoiInstPort: SeqAction[Option[Int]] = TcsEpics.instance.gsaoiPort

  override def getF2InstPort: SeqAction[Option[Int]] = TcsEpics.instance.f2Port
}