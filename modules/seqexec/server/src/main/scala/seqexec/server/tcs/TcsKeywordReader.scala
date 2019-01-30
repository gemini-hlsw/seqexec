// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.{Apply, Eq}
import cats.effect.IO
import cats.implicits._
import gem.math.Angle
import edu.gemini.spModel.core.Wavelength
import edu.gemini.spModel.core.Wavelength
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import monocle.Prism
import seqexec.server.SeqActionF
import seqexec.server.keywords._
import squants.space.{Angstroms, Meters}

sealed trait CRFollow extends Product with Serializable

object CRFollow {
  case object On extends CRFollow
  case object Off extends CRFollow

  implicit val eq: Eq[CRFollow] = Eq.fromUniversalEquals

  def keywordValue(cr: CRFollow): String = cr match {
    case On  => "yes"
    case Off => "no"
  }

  def fromInt: Prism[Int, CRFollow] =
    Prism[Int, CRFollow] {
      case 0  => Off.some
      case 1  => On.some
      case _  => none
    } {
      case Off => 0
      case On  => 1
    }

}

trait TargetKeywordsReader[F[_]] {
  def getRA: SeqActionF[F, Option[Double]]

  def getDec: SeqActionF[F, Option[Double]]

  def getRadialVelocity: SeqActionF[F, Option[Double]]

  def getParallax: SeqActionF[F, Option[Double]]

  def getWavelength: SeqActionF[F, Option[Wavelength]]

  def getEpoch: SeqActionF[F, Option[Double]]

  def getEquinox: SeqActionF[F, Option[Double]]

  def getFrame: SeqActionF[F, Option[String]]

  def getObjectName: SeqActionF[F, Option[String]]

  def getProperMotionDec: SeqActionF[F, Option[Double]]

  def getProperMotionRA: SeqActionF[F, Option[Double]]

}

trait TcsKeywordsReader[F[_]] {
  def getHourAngle: SeqActionF[F, Option[String]]

  def getLocalTime: SeqActionF[F, Option[String]]

  def getTrackingFrame: SeqActionF[F, Option[String]]

  def getTrackingEpoch: SeqActionF[F, Option[Double]]

  def getTrackingEquinox: SeqActionF[F, Option[Double]]

  def getTrackingDec: SeqActionF[F, Option[Double]]

  def getTrackingRA: SeqActionF[F, Option[Double]]

  def getElevation: SeqActionF[F, Option[Double]]

  def getAzimuth: SeqActionF[F, Option[Double]]

  def getCRPositionAngle: SeqActionF[F, Option[Double]]

  def getUT: SeqActionF[F, Option[String]]

  def getDate: SeqActionF[F, Option[String]]

  def getM2Baffle: SeqActionF[F, Option[String]]

  def getM2CentralBaffle: SeqActionF[F, Option[String]]

  def getST: SeqActionF[F, Option[String]]

  def getSFRotation: SeqActionF[F, Option[Double]]

  def getSFTilt: SeqActionF[F, Option[Double]]

  def getSFLinear: SeqActionF[F, Option[Double]]

  def getInstrumentPA: SeqActionF[F, Option[Double]]

  def getXOffset: SeqActionF[F, Option[Double]]

  def getYOffset: SeqActionF[F, Option[Double]]

  def getTrackingRAOffset: SeqActionF[F, Option[Double]]

  def getTrackingDecOffset: SeqActionF[F, Option[Double]]

  def getInstrumentAA: SeqActionF[F, Option[Double]]

  def getAOFoldName: SeqActionF[F, Option[String]]

  def getSourceATarget: TargetKeywordsReader[F]

  def getPwfs1Target: TargetKeywordsReader[F]

  def getPwfs2Target: TargetKeywordsReader[F]

  def getOiwfsTarget: TargetKeywordsReader[F]

  def getAowfsTarget: TargetKeywordsReader[F]

  def getGwfs1Target: TargetKeywordsReader[F]

  def getGwfs2Target: TargetKeywordsReader[F]

  def getGwfs3Target: TargetKeywordsReader[F]

  def getGwfs4Target: TargetKeywordsReader[F]

  def getAirMass: SeqActionF[F, Option[Double]]

  def getStartAirMass: SeqActionF[F, Option[Double]]

  def getEndAirMass: SeqActionF[F, Option[Double]]

  def getCarouselMode: SeqActionF[F, Option[String]]

  def getM2UserFocusOffset: SeqActionF[F, Option[Double]]

  def getParallacticAngle: SeqActionF[F, Option[Angle]]

  def getPwfs1Freq: SeqActionF[F, Option[Double]]

  def getPwfs2Freq: SeqActionF[F, Option[Double]]

  def getOiwfsFreq: SeqActionF[F, Option[Double]]

  def getGmosInstPort: SeqActionF[F, Option[Int]]

  def getGnirsInstPort: SeqActionF[F, Option[Int]]

  def getGpiInstPort: SeqActionF[F, Option[Int]]

  def getNiriInstPort: SeqActionF[F, Option[Int]]

  def getNifsInstPort: SeqActionF[F, Option[Int]]

  def getGsaoiInstPort: SeqActionF[F, Option[Int]]

  def getF2InstPort: SeqActionF[F, Option[Int]]

  def getCRFollow: SeqActionF[F, Option[CRFollow]]

}

object DummyTargetKeywordsReader extends TargetKeywordsReader[IO] {

  override def getRA: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getDec: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getRadialVelocity: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getParallax: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getWavelength: SeqActionF[IO, Option[Wavelength]] = Wavelength(Meters(0.0)).toSeqAction

  override def getEpoch: SeqActionF[IO, Option[Double]] = 2000.0.toSeqAction

  override def getEquinox: SeqActionF[IO, Option[Double]] = 2000.0.toSeqAction

  override def getFrame: SeqActionF[IO, Option[String]] = "FK5".toSeqAction

  override def getObjectName: SeqActionF[IO, Option[String]] = "Dummy".toSeqAction

  override def getProperMotionDec: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getProperMotionRA: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

}

object DummyTcsKeywordsReader extends TcsKeywordsReader[IO] {

  override def getHourAngle: SeqActionF[IO, Option[String]] = "00:00:00".toSeqAction

  override def getLocalTime: SeqActionF[IO, Option[String]] = "00:00:00".toSeqAction

  override def getTrackingFrame: SeqActionF[IO, Option[String]] = "FK5".toSeqAction

  override def getTrackingEpoch: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getTrackingEquinox: SeqActionF[IO, Option[Double]] = 2000.0.toSeqAction

  override def getTrackingDec: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getTrackingRA: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getElevation: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getAzimuth: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getCRPositionAngle: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getUT: SeqActionF[IO, Option[String]] = "00:00:00".toSeqAction

  override def getDate: SeqActionF[IO, Option[String]] = LocalDate.now.format(DateTimeFormatter.ISO_LOCAL_DATE).toSeqAction

  override def getM2Baffle: SeqActionF[IO, Option[String]] = "OUT".toSeqAction

  override def getM2CentralBaffle: SeqActionF[IO, Option[String]] = "OUT".toSeqAction

  override def getST: SeqActionF[IO, Option[String]] = "00:00:00".toSeqAction

  override def getSFRotation: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getSFTilt: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getSFLinear: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getInstrumentPA: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getXOffset: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getYOffset: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getInstrumentAA: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getAOFoldName: SeqActionF[IO, Option[String]] = "OUT".toSeqAction

  override def getSourceATarget: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getAirMass: SeqActionF[IO, Option[Double]] = 1.0.toSeqAction

  override def getStartAirMass: SeqActionF[IO, Option[Double]] = 1.0.toSeqAction

  override def getEndAirMass: SeqActionF[IO, Option[Double]] = 1.0.toSeqAction

  override def getParallacticAngle: SeqActionF[IO, Option[Angle]] = Some(Angle.fromDoubleDegrees(0)).toSeqActionO

  override def getTrackingRAOffset: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getTrackingDecOffset: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getPwfs1Target: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getPwfs2Target: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getOiwfsTarget: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getAowfsTarget: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getGwfs1Target: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getGwfs2Target: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getGwfs3Target: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getGwfs4Target: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getM2UserFocusOffset: SeqActionF[IO, Option[Double]] = 0.0.toSeqAction

  override def getPwfs1Freq: SeqActionF[IO, Option[Double]] = -9999.0.toSeqAction

  override def getPwfs2Freq: SeqActionF[IO, Option[Double]] = -9999.0.toSeqAction

  override def getOiwfsFreq: SeqActionF[IO, Option[Double]] = -9999.0.toSeqAction

  override def getCarouselMode: SeqActionF[IO, Option[String]] = "Basic".toSeqAction

  override def getGmosInstPort: SeqActionF[IO, Option[Int]] = 0.toSeqAction

  override def getGnirsInstPort: SeqActionF[IO, Option[Int]] = 0.toSeqAction

  override def getGpiInstPort: SeqActionF[IO, Option[Int]] = 0.toSeqAction

  override def getNiriInstPort: SeqActionF[IO, Option[Int]] = 0.toSeqAction

  override def getNifsInstPort: SeqActionF[IO, Option[Int]] = 0.toSeqAction

  override def getGsaoiInstPort: SeqActionF[IO, Option[Int]] = 0.toSeqAction

  override def getF2InstPort: SeqActionF[IO, Option[Int]] = 0.toSeqAction

  override def getCRFollow: SeqActionF[IO, Option[CRFollow]] = (CRFollow.Off: CRFollow).toSeqAction
}

object TcsKeywordsReaderImpl extends TcsKeywordsReader[IO] {

  override def getHourAngle: SeqActionF[IO, Option[String]] = TcsEpics.instance.hourAngle.toSeqActionO

  override def getLocalTime: SeqActionF[IO, Option[String]] = TcsEpics.instance.localTime.toSeqActionO

  override def getTrackingFrame: SeqActionF[IO, Option[String]] = TcsEpics.instance.trackingFrame.toSeqActionO

  override def getTrackingEpoch: SeqActionF[IO, Option[Double]] = TcsEpics.instance.trackingEpoch.toSeqActionO

  private def translateEpoch(v: Option[String]): Option[Double] = v.flatMap(x => Either.catchNonFatal { x.drop(1).toDouble }.toOption)

  override def getTrackingEquinox: SeqActionF[IO, Option[Double]] = translateEpoch(TcsEpics.instance.trackingEquinox).toSeqActionO

  override def getTrackingDec: SeqActionF[IO, Option[Double]] = TcsEpics.instance.trackingDec.toSeqActionO

  override def getTrackingRA: SeqActionF[IO, Option[Double]] = TcsEpics.instance.trackingRA.toSeqActionO

  override def getElevation: SeqActionF[IO, Option[Double]] = TcsEpics.instance.elevation.toSeqActionO

  override def getAzimuth: SeqActionF[IO, Option[Double]] = TcsEpics.instance.azimuth.toSeqActionO

  override def getCRPositionAngle: SeqActionF[IO, Option[Double]] = TcsEpics.instance.crPositionAngle.toSeqActionO

  override def getUT: SeqActionF[IO, Option[String]] = TcsEpics.instance.ut.toSeqActionO

  override def getDate: SeqActionF[IO, Option[String]] = TcsEpics.instance.date.toSeqActionO

  override def getM2Baffle: SeqActionF[IO, Option[String]] = TcsEpics.instance.m2Baffle.toSeqActionO

  override def getM2CentralBaffle: SeqActionF[IO, Option[String]] = TcsEpics.instance.m2CentralBaffle.toSeqActionO

  override def getST: SeqActionF[IO, Option[String]] = TcsEpics.instance.st.toSeqActionO

  override def getSFRotation: SeqActionF[IO, Option[Double]] = TcsEpics.instance.sfRotation.toSeqActionO

  override def getSFTilt: SeqActionF[IO, Option[Double]] = TcsEpics.instance.sfTilt.toSeqActionO

  override def getSFLinear: SeqActionF[IO, Option[Double]] = TcsEpics.instance.sfLinear.toSeqActionO

  override def getInstrumentPA: SeqActionF[IO, Option[Double]] = TcsEpics.instance.instrPA.toSeqActionO

  override def getGmosInstPort: SeqActionF[IO, Option[Int]] = TcsEpics.instance.gmosPort.toSeqActionO

  private val xoffIndex = 6
  private val yoffIndex = 7
  private val focalPlaneScale = 1.611443804

  override def getXOffset: SeqActionF[IO, Option[Double]] =
    TcsEpics.instance.targetA.flatMap(v => v.lift(xoffIndex).map(_*focalPlaneScale)).toSeqActionO

  override def getYOffset: SeqActionF[IO, Option[Double]] =
    TcsEpics.instance.targetA.flatMap(v => v.lift(yoffIndex).map(_*focalPlaneScale)).toSeqActionO

  private val raoffIndex = 2
  private val decoffIndex = 3
  private val degreeToArcsec = 3600.0

  override def getTrackingRAOffset: SeqActionF[IO, Option[Double]] = {
    def angleRange(v: Double) = {
      val r = v % 360.0
      if (r<180.0) r + 360.0
      else if (r>=180.0) r - 360.0
      else r
    }
    def raOffset(off: Double, dec: Double): Double = angleRange(Math.toDegrees(off)) * Math.cos(dec)*degreeToArcsec

    TcsEpics.instance.targetA.flatMap(v => Apply[Option].ap2(Option(raOffset _))(v.lift(raoffIndex),v.lift(decoffIndex))).toSeqActionO
  }

  override def getTrackingDecOffset: SeqActionF[IO, Option[Double]] =
    TcsEpics.instance.targetA.flatMap(v => v.lift(decoffIndex).map(Math.toDegrees(_) * degreeToArcsec)).toSeqActionO

  override def getInstrumentAA: SeqActionF[IO, Option[Double]] = TcsEpics.instance.instrAA.toSeqActionO

  override def getAOFoldName: SeqActionF[IO, Option[String]] = TcsEpics.instance.aoFoldPosition.toSeqActionO

  private def target(t: TcsEpics.Target) = new TargetKeywordsReader[IO] {

    override def getRA: SeqActionF[IO, Option[Double]] = t.ra.toSeqActionO

    override def getDec: SeqActionF[IO, Option[Double]] = t.dec.toSeqActionO

    override def getRadialVelocity: SeqActionF[IO, Option[Double]] = t.radialVelocity.toSeqActionO

    override def getParallax: SeqActionF[IO, Option[Double]] = t.parallax.toSeqActionO

    override def getWavelength: SeqActionF[IO, Option[Wavelength]] = t.centralWavelenght.map(v => Wavelength(Angstroms[Double](v))).toSeqActionO

    override def getEpoch: SeqActionF[IO, Option[Double]] = translateEpoch(t.epoch).toSeqActionO

    override def getEquinox: SeqActionF[IO, Option[Double]] = translateEpoch(t.equinox).toSeqActionO

    override def getFrame: SeqActionF[IO, Option[String]] = t.frame.toSeqActionO

    override def getObjectName: SeqActionF[IO, Option[String]] = t.objectName.toSeqActionO

    override def getProperMotionDec: SeqActionF[IO, Option[Double]] = t.properMotionDec.toSeqActionO

    override def getProperMotionRA: SeqActionF[IO, Option[Double]] = t.properMotionRA.toSeqActionO
  }

  override def getSourceATarget: TargetKeywordsReader[IO] = target(TcsEpics.instance.sourceATarget)

  override def getAirMass: SeqActionF[IO, Option[Double]] =  TcsEpics.instance.airmass.toSeqActionO

  override def getStartAirMass: SeqActionF[IO, Option[Double]] = TcsEpics.instance.airmassStart.toSeqActionO

  override def getEndAirMass: SeqActionF[IO, Option[Double]] = TcsEpics.instance.airmassEnd.toSeqActionO

  override def getParallacticAngle: SeqActionF[IO, Option[Angle]] = TcsEpics.instance.parallacticAngle.toSeqActionO

  override def getPwfs1Target: TargetKeywordsReader[IO] = target(TcsEpics.instance.pwfs1Target)

  override def getPwfs2Target: TargetKeywordsReader[IO] = target(TcsEpics.instance.pwfs2Target)

  override def getOiwfsTarget: TargetKeywordsReader[IO] = target(TcsEpics.instance.oiwfsTarget)

  override def getAowfsTarget: TargetKeywordsReader[IO] = target(TcsEpics.instance.pwfs2Target)

  override def getGwfs1Target: TargetKeywordsReader[IO] = target(TcsEpics.instance.gwfs1Target)

  override def getGwfs2Target: TargetKeywordsReader[IO] = target(TcsEpics.instance.gwfs2Target)

  override def getGwfs3Target: TargetKeywordsReader[IO] = target(TcsEpics.instance.gwfs3Target)

  override def getGwfs4Target: TargetKeywordsReader[IO] = target(TcsEpics.instance.gwfs4Target)

  override def getM2UserFocusOffset: SeqActionF[IO, Option[Double]] = TcsEpics.instance.m2UserFocusOffset.toSeqActionO

  private def calcFrequency(t: Double): Double = if (t > 0.0) 1.0 / t else -9999.0

  override def getPwfs1Freq: SeqActionF[IO, Option[Double]] = TcsEpics.instance.pwfs1IntegrationTime.map(calcFrequency).toSeqActionO

  override def getPwfs2Freq: SeqActionF[IO, Option[Double]] = TcsEpics.instance.pwfs2IntegrationTime.map(calcFrequency).toSeqActionO

  override def getOiwfsFreq: SeqActionF[IO, Option[Double]] = TcsEpics.instance.oiwfsIntegrationTime.map(calcFrequency).toSeqActionO

  override def getCarouselMode: SeqActionF[IO, Option[String]] = TcsEpics.instance.carouselMode.toSeqActionO

  override def getGnirsInstPort: SeqActionF[IO, Option[Int]] = TcsEpics.instance.gnirsPort.toSeqActionO

  override def getGpiInstPort: SeqActionF[IO, Option[Int]] = TcsEpics.instance.gpiPort.toSeqActionO

  override def getNiriInstPort: SeqActionF[IO, Option[Int]] = TcsEpics.instance.niriPort.toSeqActionO

  override def getNifsInstPort: SeqActionF[IO, Option[Int]] = TcsEpics.instance.nifsPort.toSeqActionO

  override def getGsaoiInstPort: SeqActionF[IO, Option[Int]] = TcsEpics.instance.gsaoiPort.toSeqActionO

  override def getF2InstPort: SeqActionF[IO, Option[Int]] = TcsEpics.instance.f2Port.toSeqActionO

  override def getCRFollow: SeqActionF[IO, Option[CRFollow]] =
    TcsEpics.instance.crFollow.flatMap(CRFollow.fromInt.getOption).toSeqActionO
}
