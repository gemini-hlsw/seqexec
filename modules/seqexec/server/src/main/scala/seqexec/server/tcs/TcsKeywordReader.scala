// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.{Apply, Eq}
import cats.effect.IO
import cats.implicits._
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.data.OptionT
import monocle.Prism
import seqexec.server.keywords._
import squants.space._

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
  def getRA: F[Double]

  def getDec: F[Double]

  def getRadialVelocity: F[Double]

  def getParallax: F[Double]

  def getWavelength: F[Double]

  def getEpoch: F[Double]

  def getEquinox: F[Double]

  def getFrame: F[String]

  def getObjectName: F[String]

  def getProperMotionDec: F[Double]

  def getProperMotionRA: F[Double]

}

trait TcsKeywordsReader[F[_]] {
  def getHourAngle: F[String]

  def getLocalTime: F[String]

  def getTrackingFrame: F[String]

  def getTrackingEpoch: F[Double]

  def getTrackingEquinox: F[Double]

  def getTrackingDec: F[Double]

  def getTrackingRA: F[Double]

  def getElevation: F[Double]

  def getAzimuth: F[Double]

  def getCRPositionAngle: F[Double]

  def getUT: F[String]

  def getDate: F[String]

  def getM2Baffle: F[String]

  def getM2CentralBaffle: F[String]

  def getST: F[String]

  def getSFRotation: F[Double]

  def getSFTilt: F[Double]

  def getSFLinear: F[Double]

  def getInstrumentPA: F[Double]

  def getXOffset: F[Double]

  def getYOffset: F[Double]

  def getPOffset: F[Double]

  def getQOffset: F[Double]

  def getRaOffset: F[Double]

  def getDecOffset: F[Double]

  def getTrackingRAOffset: F[Double]

  def getTrackingDecOffset: F[Double]

  def getInstrumentAA: F[Double]

  def getAOFoldName: F[String]

  def getSourceATarget: TargetKeywordsReader[F]

  def getPwfs1Target: TargetKeywordsReader[F]

  def getPwfs2Target: TargetKeywordsReader[F]

  def getOiwfsTarget: TargetKeywordsReader[F]

  def getAowfsTarget: TargetKeywordsReader[F]

  def getGwfs1Target: TargetKeywordsReader[F]

  def getGwfs2Target: TargetKeywordsReader[F]

  def getGwfs3Target: TargetKeywordsReader[F]

  def getGwfs4Target: TargetKeywordsReader[F]

  def getAirMass: F[Double]

  def getStartAirMass: F[Double]

  def getEndAirMass: F[Double]

  def getCarouselMode: F[String]

  def getM2UserFocusOffset: F[Double]

  def getParallacticAngle: F[Option[Angle]]

  def getPwfs1Freq: F[Double]

  def getPwfs2Freq: F[Double]

  def getOiwfsFreq: F[Double]

  def getGmosInstPort: F[Int]

  def getGnirsInstPort: F[Int]

  def getGpiInstPort: F[Int]

  def getNiriInstPort: F[Int]

  def getNifsInstPort: F[Int]

  def getGsaoiInstPort: F[Int]

  def getF2InstPort: F[Int]

  def getCRFollow: F[Option[CRFollow]]

}

object DummyTargetKeywordsReader extends TargetKeywordsReader[IO] {

  override def getRA: IO[Double] = IO(0.0)

  override def getDec: IO[Double] = IO(0.0)

  override def getRadialVelocity: IO[Double] = IO(0.0)

  override def getParallax: IO[Double] = IO(0.0)

  override def getWavelength: IO[Double] = IO(0.0)

  override def getEpoch: IO[Double] = IO(2000.0)

  override def getEquinox: IO[Double] = IO(2000.0)

  override def getFrame: IO[String] = IO("FK5")

  override def getObjectName: IO[String] = IO("Dummy")

  override def getProperMotionDec: IO[Double] = IO(0.0)

  override def getProperMotionRA: IO[Double] = IO(0.0)

}

object DummyTcsKeywordsReader extends TcsKeywordsReader[IO] {

  override def getHourAngle: IO[String] = IO("00:00:00")

  override def getLocalTime: IO[String] = IO("00:00:00")

  override def getTrackingFrame: IO[String] = IO("FK5")

  override def getTrackingEpoch: IO[Double] = IO(0.0)

  override def getTrackingEquinox: IO[Double] = IO(2000.0)

  override def getTrackingDec: IO[Double] = IO(0.0)

  override def getTrackingRA: IO[Double] = IO(0.0)

  override def getElevation: IO[Double] = IO(0.0)

  override def getAzimuth: IO[Double] = IO(0.0)

  override def getCRPositionAngle: IO[Double] = IO(0.0)

  override def getUT: IO[String] = IO("00:00:00")

  override def getDate: IO[String] = IO(LocalDate.now.format(DateTimeFormatter.ISO_LOCAL_DATE))

  override def getM2Baffle: IO[String] = IO("OUT")

  override def getM2CentralBaffle: IO[String] = IO("OUT")

  override def getST: IO[String] = IO("00:00:00")

  override def getSFRotation: IO[Double] = IO(0.0)

  override def getSFTilt: IO[Double] = IO(0.0)

  override def getSFLinear: IO[Double] = IO(0.0)

  override def getInstrumentPA: IO[Double] = IO(0.0)

  override def getXOffset: IO[Double] = IO(0.0)

  override def getYOffset: IO[Double] = IO(0.0)

  override def getInstrumentAA: IO[Double] = IO(0.0)

  override def getAOFoldName: IO[String] = IO("OUT")

  override def getSourceATarget: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getAirMass: IO[Double] = IO(1.0)

  override def getStartAirMass: IO[Double] = IO(1.0)

  override def getEndAirMass: IO[Double] = IO(1.0)

  override def getParallacticAngle: IO[Option[Angle]] = IO(Some(Arcseconds(0.0)))

  override def getTrackingRAOffset: IO[Double] = IO(0.0)

  override def getTrackingDecOffset: IO[Double] = IO(0.0)

  override def getPwfs1Target: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getPwfs2Target: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getOiwfsTarget: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getAowfsTarget: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getGwfs1Target: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getGwfs2Target: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getGwfs3Target: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getGwfs4Target: TargetKeywordsReader[IO] = DummyTargetKeywordsReader

  override def getM2UserFocusOffset: IO[Double] = IO(0.0)

  override def getPwfs1Freq: IO[Double] = IO(-9999.0)

  override def getPwfs2Freq: IO[Double] = IO(-9999.0)

  override def getOiwfsFreq: IO[Double] = IO(-9999.0)

  override def getCarouselMode: IO[String] = IO("Basic")

  override def getGmosInstPort: IO[Int] = IO(0)

  override def getGnirsInstPort: IO[Int] = IO(0)

  override def getGpiInstPort: IO[Int] = IO(0)

  override def getNiriInstPort: IO[Int] = IO(0)

  override def getNifsInstPort: IO[Int] = IO(0)

  override def getGsaoiInstPort: IO[Int] = IO(0)

  override def getF2InstPort: IO[Int] = IO(0)

  override def getCRFollow: IO[Option[CRFollow]] = IO(CRFollow.Off.some)

  override def getPOffset: IO[Double] = IO(0.0)

  override def getQOffset: IO[Double] = IO(0.0)

  override def getRaOffset: IO[Double] = IO(0.0)

  override def getDecOffset: IO[Double] = IO(0.0)
}

object TcsKeywordsReaderImpl extends TcsKeywordsReader[IO] {

  override def getHourAngle: IO[String] = TcsEpics.instance.hourAngle.safeValOrDefault

  override def getLocalTime: IO[String] = TcsEpics.instance.localTime.safeValOrDefault

  override def getTrackingFrame: IO[String] = TcsEpics.instance.trackingFrame.safeValOrDefault

  override def getTrackingEpoch: IO[Double] = TcsEpics.instance.trackingEpoch.safeValOrDefault

  private def translateEpoch(v: Option[String]): Option[Double] = v.flatMap(x => Either.catchNonFatal { x.drop(1).toDouble }.toOption)

  override def getTrackingEquinox: IO[Double] = TcsEpics.instance.trackingEquinox.map(translateEpoch).safeValOrDefault

  override def getTrackingDec: IO[Double] = TcsEpics.instance.trackingDec.safeValOrDefault

  override def getTrackingRA: IO[Double] = TcsEpics.instance.trackingRA.safeValOrDefault

  override def getElevation: IO[Double] = TcsEpics.instance.elevation.safeValOrDefault

  override def getAzimuth: IO[Double] = TcsEpics.instance.azimuth.safeValOrDefault

  override def getCRPositionAngle: IO[Double] = TcsEpics.instance.crPositionAngle.safeValOrDefault

  override def getUT: IO[String] = TcsEpics.instance.ut.safeValOrDefault

  override def getDate: IO[String] = TcsEpics.instance.date.safeValOrDefault

  override def getM2Baffle: IO[String] = TcsEpics.instance.m2Baffle.safeValOrDefault

  override def getM2CentralBaffle: IO[String] = TcsEpics.instance.m2CentralBaffle.safeValOrDefault

  override def getST: IO[String] = TcsEpics.instance.st.safeValOrDefault

  override def getSFRotation: IO[Double] = TcsEpics.instance.sfRotation.safeValOrDefault

  override def getSFTilt: IO[Double] = TcsEpics.instance.sfTilt.safeValOrDefault

  override def getSFLinear: IO[Double] = TcsEpics.instance.sfLinear.safeValOrDefault

  override def getInstrumentPA: IO[Double] = TcsEpics.instance.instrPA.safeValOrDefault

  override def getGmosInstPort: IO[Int] = TcsEpics.instance.gmosPort.safeValOrDefault

  private val xoffIndex = 6
  private val yoffIndex = 7

  def getXOffsetOption: IO[Option[Angle]] =
    TcsEpics.instance.targetA.map(_.flatMap(v => v.lift(xoffIndex).map(x => Millimeters(x)*FOCAL_PLANE_SCALE)))

  override def getXOffset: IO[Double] =
    getXOffsetOption.safeVal.map(_.map(_.toArcseconds).getOrElse(DefaultHeaderValue[Double].default))

  def getYOffsetOption: IO[Option[Angle]] =
    TcsEpics.instance.targetA.map(_.flatMap(v => v.lift(yoffIndex).map(x => Millimeters(x)*FOCAL_PLANE_SCALE)))

  override def getYOffset: IO[Double] =
    getYOffsetOption.safeVal.map(_.map(_.toArcseconds).getOrElse(DefaultHeaderValue[Double].default))

  private val raoffIndex = 2
  private val decoffIndex = 3

  def normalizePositiveAngle(v: Angle): Angle = {
    val r = v.value % 360.0
    Degrees(
      if (r < 0.0) r + 360.0
      else r
    )
  }
  def normalizeSignedAngle(v: Angle): Angle = {
    val r = v.value % 360.0
    Degrees(
      if (r < -180.0) r + 360.0
      else if (r >= 180.0) r - 360.0
      else r
    )
  }

  override def getTrackingRAOffset: IO[Double] = {
    def raOffset(off: Angle, dec: Angle): Angle = normalizeSignedAngle(off) * dec.cos

    TcsEpics.instance.targetA.map(_.flatMap(v =>
      Apply[Option].ap2(Option(raOffset _))(v.lift(raoffIndex).map(Radians(_)),v.lift(decoffIndex).map(Radians(_)))))
      .safeVal.map(_.map(_.toArcseconds).getOrElse(DefaultHeaderValue[Double].default))
  }

  override def getTrackingDecOffset: IO[Double] =
    TcsEpics.instance.targetA.map(_.flatMap(v => v.lift(decoffIndex).map(Radians(_)))).safeVal
      .map(_.map(_.toArcseconds).getOrElse(DefaultHeaderValue[Double].default))

  override def getInstrumentAA: IO[Double] = TcsEpics.instance.instrAA.safeValOrDefault

  override def getAOFoldName: IO[String] = TcsEpics.instance.aoFoldPosition.safeValOrDefault

  private def target(t: TcsEpics.Target[IO]) = new TargetKeywordsReader[IO] {

    override def getRA: IO[Double] = t.ra.safeValOrDefault

    override def getDec: IO[Double] = t.dec.safeValOrDefault

    override def getRadialVelocity: IO[Double] = t.radialVelocity.safeValOrDefault

    override def getParallax: IO[Double] = t.parallax.safeValOrDefault

    override def getWavelength: IO[Double] = t.centralWavelenght.safeValOrDefault

    override def getEpoch: IO[Double] = t.epoch.map(translateEpoch).safeValOrDefault

    override def getEquinox: IO[Double] = t.equinox.map(translateEpoch).safeValOrDefault

    override def getFrame: IO[String] = t.frame.safeValOrDefault

    override def getObjectName: IO[String] = t.objectName.safeValOrDefault

    override def getProperMotionDec: IO[Double] = t.properMotionDec.safeValOrDefault

    override def getProperMotionRA: IO[Double] = t.properMotionRA.safeValOrDefault
  }

  override def getSourceATarget: TargetKeywordsReader[IO] = target(TcsEpics.instance.sourceATarget)

  override def getAirMass: IO[Double] =  TcsEpics.instance.airmass.safeValOrDefault

  override def getStartAirMass: IO[Double] = TcsEpics.instance.airmassStart.safeValOrDefault

  override def getEndAirMass: IO[Double] = TcsEpics.instance.airmassEnd.safeValOrDefault

  override def getParallacticAngle: IO[Option[Angle]] =
    TcsEpics.instance.parallacticAngle.map(_.map(normalizeSignedAngle)).safeVal

  override def getPwfs1Target: TargetKeywordsReader[IO] = target(TcsEpics.instance.pwfs1Target)

  override def getPwfs2Target: TargetKeywordsReader[IO] = target(TcsEpics.instance.pwfs2Target)

  override def getOiwfsTarget: TargetKeywordsReader[IO] = target(TcsEpics.instance.oiwfsTarget)

  override def getAowfsTarget: TargetKeywordsReader[IO] = target(TcsEpics.instance.pwfs2Target)

  override def getGwfs1Target: TargetKeywordsReader[IO] = target(TcsEpics.instance.gwfs1Target)

  override def getGwfs2Target: TargetKeywordsReader[IO] = target(TcsEpics.instance.gwfs2Target)

  override def getGwfs3Target: TargetKeywordsReader[IO] = target(TcsEpics.instance.gwfs3Target)

  override def getGwfs4Target: TargetKeywordsReader[IO] = target(TcsEpics.instance.gwfs4Target)

  override def getM2UserFocusOffset: IO[Double] = TcsEpics.instance.m2UserFocusOffset.safeValOrDefault

  private def calcFrequency(t: Double): Double = if (t > 0.0) 1.0 / t else DefaultHeaderValue[Double].default

  override def getPwfs1Freq: IO[Double] = TcsEpics.instance.pwfs1IntegrationTime.map(_.map(calcFrequency)).safeValOrDefault

  override def getPwfs2Freq: IO[Double] = TcsEpics.instance.pwfs2IntegrationTime.map(_.map(calcFrequency)).safeValOrDefault

  override def getOiwfsFreq: IO[Double] = TcsEpics.instance.oiwfsIntegrationTime.map(_.map(calcFrequency)).safeValOrDefault

  override def getCarouselMode: IO[String] = TcsEpics.instance.carouselMode.safeValOrDefault

  override def getGnirsInstPort: IO[Int] = TcsEpics.instance.gnirsPort.safeValOrDefault

  override def getGpiInstPort: IO[Int] = TcsEpics.instance.gpiPort.safeValOrDefault

  override def getNiriInstPort: IO[Int] = TcsEpics.instance.niriPort.safeValOrDefault

  override def getNifsInstPort: IO[Int] = TcsEpics.instance.nifsPort.safeValOrDefault

  override def getGsaoiInstPort: IO[Int] = TcsEpics.instance.gsaoiPort.safeValOrDefault

  override def getF2InstPort: IO[Int] = TcsEpics.instance.f2Port.safeValOrDefault

  override def getCRFollow: IO[Option[CRFollow]] = TcsEpics.instance.crFollow.safeVal.map(_.flatMap(CRFollow.fromInt.getOption))

  def getPOffsetOption: IO[Option[Angle]] = (
    for {
      xoff <- OptionT(getXOffsetOption)
      yoff <- OptionT(getYOffsetOption)
      iaa  <- OptionT(TcsEpics.instance.instrAA).map(Degrees(_))
    } yield  -xoff * iaa.cos + yoff * iaa.sin
  ).value

  override def getPOffset: IO[Double] = getPOffsetOption.map(_.map(_.toArcseconds)).safeValOrDefault

  def getQOffsetOption: IO[Option[Angle]] = (
    for {
      xoff <- OptionT(getXOffsetOption)
      yoff <- OptionT(getYOffsetOption)
      iaa  <- OptionT(TcsEpics.instance.instrAA).map(Degrees(_))
    } yield  -xoff * iaa.sin - yoff * iaa.cos
  ).value

  override def getQOffset: IO[Double] = getQOffsetOption.map(_.map(_.toArcseconds)).safeValOrDefault

  override def getRaOffset: IO[Double] = (
    for {
      p <- OptionT(getPOffsetOption)
      q <- OptionT(getQOffsetOption)
      ipa <- OptionT(TcsEpics.instance.instrPA).map(Degrees(_))
    } yield p * ipa.cos + q * ipa.sin
  ).map(_.toArcseconds).value.safeValOrDefault

  override def getDecOffset: IO[Double] = (
    for {
      p <- OptionT(getPOffsetOption)
      q <- OptionT(getQOffsetOption)
      ipa <- OptionT(TcsEpics.instance.instrPA).map(Degrees(_))
    } yield -p * ipa.sin + q * ipa.cos
  ).map(_.toArcseconds).value.safeValOrDefault

}
