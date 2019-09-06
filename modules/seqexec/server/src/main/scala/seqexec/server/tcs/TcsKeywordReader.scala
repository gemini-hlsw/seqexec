// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats._
import cats.effect.Sync
import cats.data.OptionT
import cats.data.Nested
import cats.implicits._
import gsp.math.syntax.string._
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import monocle.Prism
import seqexec.server.keywords._
import seqexec.server.tcs.TcsEpics.VirtualGemsTelescope
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
  def ra: F[Double]

  def dec: F[Double]

  def radialVelocity: F[Double]

  def parallax: F[Double]

  def wavelength: F[Double]

  def epoch: F[Double]

  def equinox: F[Double]

  def frame: F[String]

  def objectName: F[String]

  def properMotionDec: F[Double]

  def properMotionRA: F[Double]

}

trait TcsKeywordsReader[F[_]] {
  def hourAngle: F[String]

  def localTime: F[String]

  def trackingFrame: F[String]

  def trackingEpoch: F[Double]

  def trackingEquinox: F[Double]

  def trackingDec: F[Double]

  def trackingRA: F[Double]

  def elevation: F[Double]

  def azimuth: F[Double]

  def crPositionAngle: F[Double]

  def ut: F[String]

  def date: F[String]

  def m2Baffle: F[String]

  def m2CentralBaffle: F[String]

  def st: F[String]

  def sfRotation: F[Double]

  def sfTilt: F[Double]

  def sfLinear: F[Double]

  def instrumentPA: F[Double]

  def xOffset: F[Double]

  def yOffset: F[Double]

  def pOffset: F[Double]

  def qOffset: F[Double]

  def raOffset: F[Double]

  def decOffset: F[Double]

  def trackingRAOffset: F[Double]

  def trackingDecOffset: F[Double]

  def instrumentAA: F[Double]

  def aoFoldName: F[String]

  def sourceATarget: TargetKeywordsReader[F]

  def pwfs1Target: TargetKeywordsReader[F]

  def pwfs2Target: TargetKeywordsReader[F]

  def oiwfsTarget: TargetKeywordsReader[F]

  def aowfsTarget: TargetKeywordsReader[F]

  def gwfs1Target: TargetKeywordsReader[F]

  def gwfs2Target: TargetKeywordsReader[F]

  def gwfs3Target: TargetKeywordsReader[F]

  def gwfs4Target: TargetKeywordsReader[F]

  def gwfsTarget(v: VirtualGemsTelescope): TargetKeywordsReader[F]

  def gwfsMap: F[Map[VirtualGemsTelescope, GemsSource]]

  def airMass: F[Double]

  def startAirMass: F[Double]

  def endAirMass: F[Double]

  def carouselMode: F[String]

  def m2UserFocusOffset: F[Double]

  def parallacticAngle: F[Angle]

  def pwfs1Freq: F[Double]

  def pwfs2Freq: F[Double]

  def oiwfsFreq: F[Double]

  def gmosInstPort: F[Int]

  def gnirsInstPort: F[Int]

  def gpiInstPort: F[Int]

  def niriInstPort: F[Int]

  def nifsInstPort: F[Int]

  def gsaoiInstPort: F[Int]

  def f2InstPort: F[Int]

  def crFollow: F[Option[CRFollow]]

}

trait TcsKeywordDefaults {
  implicit val OffsetDirectionDefaultValue: DefaultHeaderValue[Angle] =
    DefaultHeaderValue[Double].map(Degrees(_))

}

object DummyTargetKeywordsReader {
  def apply[F[_]: Applicative]: TargetKeywordsReader[F] = new TargetKeywordsReader[F] {

    override def ra: F[Double] = 0.0.pure[F]

    override def dec: F[Double] = 0.0.pure[F]

    override def radialVelocity: F[Double] = 0.0.pure[F]

    override def parallax: F[Double] = 0.0.pure[F]

    override def wavelength: F[Double] = 0.0.pure[F]

    override def epoch: F[Double] = 2000.0.pure[F]

    override def equinox: F[Double] = 2000.0.pure[F]

    override def frame: F[String] = "FK5".pure[F]

    override def objectName: F[String] = "Dummy".pure[F]

    override def properMotionDec: F[Double] = 0.0.pure[F]

    override def properMotionRA: F[Double] = 0.0.pure[F]
  }
}

object DummyTcsKeywordsReader {
  def apply[F[_]: Applicative]: TcsKeywordsReader[F] = new TcsKeywordsReader[F] {

    override def hourAngle: F[String] = "00:00:00".pure[F]

    override def localTime: F[String] = "00:00:00".pure[F]

    override def trackingFrame: F[String] = "FK5".pure[F]

    override def trackingEpoch: F[Double] = 0.0.pure[F]

    override def trackingEquinox: F[Double] = 2000.0.pure[F]

    override def trackingDec: F[Double] = 0.0.pure[F]

    override def trackingRA: F[Double] = 0.0.pure[F]

    override def elevation: F[Double] = 0.0.pure[F]

    override def azimuth: F[Double] = 0.0.pure[F]

    override def crPositionAngle: F[Double] = 0.0.pure[F]

    override def ut: F[String] = "00:00:00".pure[F]

    override def date: F[String] = LocalDate.of(2019, 1, 1).format(DateTimeFormatter.ISO_LOCAL_DATE).pure[F]

    override def m2Baffle: F[String] = "OUT".pure[F]

    override def m2CentralBaffle: F[String] = "OUT".pure[F]

    override def st: F[String] = "00:00:00".pure[F]

    override def sfRotation: F[Double] = 0.0.pure[F]

    override def sfTilt: F[Double] = 0.0.pure[F]

    override def sfLinear: F[Double] = 0.0.pure[F]

    override def instrumentPA: F[Double] = 0.0.pure[F]

    override def xOffset: F[Double] = 0.0.pure[F]

    override def yOffset: F[Double] = 0.0.pure[F]

    override def instrumentAA: F[Double] = 0.0.pure[F]

    override def aoFoldName: F[String] = "OUT".pure[F]

    override def sourceATarget: TargetKeywordsReader[F] = DummyTargetKeywordsReader[F]

    override def airMass: F[Double] = 1.0.pure[F]

    override def startAirMass: F[Double] = 1.0.pure[F]

    override def endAirMass: F[Double] = 1.0.pure[F]

    override def parallacticAngle: F[Angle] = Arcseconds(0.0).pure[F]

    override def trackingRAOffset: F[Double] = 0.0.pure[F]

    override def trackingDecOffset: F[Double] = 0.0.pure[F]

    override def pwfs1Target: TargetKeywordsReader[F] = DummyTargetKeywordsReader[F]

    override def pwfs2Target: TargetKeywordsReader[F] = DummyTargetKeywordsReader[F]

    override def oiwfsTarget: TargetKeywordsReader[F] = DummyTargetKeywordsReader[F]

    override def aowfsTarget: TargetKeywordsReader[F] = DummyTargetKeywordsReader[F]

    override def gwfs1Target: TargetKeywordsReader[F] = DummyTargetKeywordsReader[F]

    override def gwfs2Target: TargetKeywordsReader[F] = DummyTargetKeywordsReader[F]

    override def gwfs3Target: TargetKeywordsReader[F] = DummyTargetKeywordsReader[F]

    override def gwfs4Target: TargetKeywordsReader[F] = DummyTargetKeywordsReader[F]

    override def gwfsTarget(v: VirtualGemsTelescope): TargetKeywordsReader[F] = DummyTargetKeywordsReader[F]

    override def gwfsMap: F[Map[VirtualGemsTelescope, GemsSource]] = Map.empty[VirtualGemsTelescope, GemsSource].pure[F]

    override def m2UserFocusOffset: F[Double] = 0.0.pure[F]

    override def pwfs1Freq: F[Double] = -9999.0.pure[F]

    override def pwfs2Freq: F[Double] = -9999.0.pure[F]

    override def oiwfsFreq: F[Double] = -9999.0.pure[F]

    override def carouselMode: F[String] = "Basic".pure[F]

    override def gmosInstPort: F[Int] = 0.pure[F]

    override def gnirsInstPort: F[Int] = 0.pure[F]

    override def gpiInstPort: F[Int] = 0.pure[F]

    override def niriInstPort: F[Int] = 0.pure[F]

    override def nifsInstPort: F[Int] = 0.pure[F]

    override def gsaoiInstPort: F[Int] = 0.pure[F]

    override def f2InstPort: F[Int] = 0.pure[F]

    override def crFollow: F[Option[CRFollow]] = CRFollow.Off.some.pure[F].widen[Option[CRFollow]]

    override def pOffset: F[Double] = 0.0.pure[F]

    override def qOffset: F[Double] = 0.0.pure[F]

    override def raOffset: F[Double] = 0.0.pure[F]

    override def decOffset: F[Double] = 0.0.pure[F]
  }
}

object TcsKeywordsReaderEpics extends TcsKeywordDefaults {
  def apply[F[_]: Sync](sys: TcsEpics[F]): TcsKeywordsReader[F] = new TcsKeywordsReader[F] {
    override def hourAngle: F[String] = sys.hourAngle.safeValOrDefault

    override def localTime: F[String] = sys.localTime.safeValOrDefault

    override def trackingFrame: F[String] = sys.trackingFrame.safeValOrDefault

    override def trackingEpoch: F[Double] = sys.trackingEpoch.safeValOrDefault

    private def translateEpoch(v: String): Option[Double] = v.drop(1).parseDoubleOption

    override def trackingEquinox: F[Double] = sys.trackingEquinox.map(translateEpoch).safeValOrDefault

    override def trackingDec: F[Double] = sys.trackingDec.safeValOrDefault

    override def trackingRA: F[Double] = sys.trackingRA.safeValOrDefault

    override def elevation: F[Double] = sys.elevation.safeValOrDefault

    override def azimuth: F[Double] = sys.azimuth.safeValOrDefault

    override def crPositionAngle: F[Double] = sys.crPositionAngle.safeValOrDefault

    override def ut: F[String] = sys.ut.safeValOrDefault

    override def date: F[String] = sys.date.safeValOrDefault

    override def m2Baffle: F[String] = sys.m2Baffle.safeValOrDefault

    override def m2CentralBaffle: F[String] = sys.m2CentralBaffle.safeValOrDefault

    override def st: F[String] = sys.st.safeValOrDefault

    override def sfRotation: F[Double] = sys.sfRotation.safeValOrDefault

    override def sfTilt: F[Double] = sys.sfTilt.safeValOrDefault

    override def sfLinear: F[Double] = sys.sfLinear.safeValOrDefault

    override def instrumentPA: F[Double] = sys.instrPA.safeValOrDefault

    override def gmosInstPort: F[Int] = sys.gmosPort.safeValOrDefault

    private val xoffIndex = 6L
    private val yoffIndex = 7L

    private def xOffsetOption: F[Option[Angle]] =
      sys.targetA
        .map(v => v.get(xoffIndex).map(x => Millimeters(x) * FOCAL_PLANE_SCALE))
        .handleError(_ => none)

    override def xOffset: F[Double] =
      Nested(xOffsetOption).map(_.toArcseconds).value.safeValOrDefault

    private def yOffsetOption: F[Option[Angle]] =
      sys.targetA
        .map(v => v.get(yoffIndex).map(x => Millimeters(x) * FOCAL_PLANE_SCALE))
        .handleError(_ => none)

    override def yOffset: F[Double] =
      Nested(yOffsetOption).map(_.toArcseconds).value.safeValOrDefault

    private val raoffIndex = 2L
    private val decoffIndex = 3L

    private def normalizeSignedAngle(v: Angle): Angle = {
      val r = v.value % 360.0
      Degrees(
        if (r < -180.0) r + 360.0
        else if (r >= 180.0) r - 360.0
        else r
      )
    }

    override def trackingRAOffset: F[Double] = {
      def raOffset(off: Angle, dec: Angle): Angle = normalizeSignedAngle(off) * dec.cos

      sys.targetA.map(v =>
        Apply[Option].ap2(Option(raOffset(_, _)))(v.get(raoffIndex).map(Radians(_)), v.get(decoffIndex).map(Radians(_)))
          .map(_.toArcseconds)
      ).safeValOrDefault
    }

    override def trackingDecOffset: F[Double] =
      sys.targetA
        .map(v => v.get(decoffIndex).map(Radians(_).toArcseconds))
        .safeValOrDefault

    override def instrumentAA: F[Double] = sys.instrAA.safeValOrDefault

    override def aoFoldName: F[String] = sys.aoFoldPosition.safeValOrDefault

    private def target(t: TcsEpics.Target[F]) = new TargetKeywordsReader[F] {

      override def ra: F[Double] = t.ra.safeValOrDefault

      override def dec: F[Double] = t.dec.safeValOrDefault

      override def radialVelocity: F[Double] = t.radialVelocity.safeValOrDefault

      override def parallax: F[Double] = t.parallax.safeValOrDefault

      override def wavelength: F[Double] = t.centralWavelenght.safeValOrDefault

      override def epoch: F[Double] = t.epoch.map(translateEpoch).safeValOrDefault

      override def equinox: F[Double] = t.equinox.map(translateEpoch).safeValOrDefault

      override def frame: F[String] = t.frame.safeValOrDefault

      override def objectName: F[String] = t.objectName.safeValOrDefault

      override def properMotionDec: F[Double] = t.properMotionDec.safeValOrDefault

      override def properMotionRA: F[Double] = t.properMotionRA.safeValOrDefault
    }

    override def sourceATarget: TargetKeywordsReader[F] = target(sys.sourceATarget)

    override def airMass: F[Double] =  sys.airmass.safeValOrDefault

    override def startAirMass: F[Double] = sys.airmassStart.safeValOrDefault

    override def endAirMass: F[Double] = sys.airmassEnd.safeValOrDefault

    override def parallacticAngle: F[Angle] = sys.parallacticAngle.map(normalizeSignedAngle).safeValOrDefault

    override def pwfs1Target: TargetKeywordsReader[F] = target(sys.pwfs1Target)

    override def pwfs2Target: TargetKeywordsReader[F] = target(sys.pwfs2Target)

    override def oiwfsTarget: TargetKeywordsReader[F] = target(sys.oiwfsTarget)

    override def aowfsTarget: TargetKeywordsReader[F] = target(sys.pwfs2Target)

    override def gwfs1Target: TargetKeywordsReader[F] = target(sys.gwfs1Target)

    override def gwfs2Target: TargetKeywordsReader[F] = target(sys.gwfs2Target)

    override def gwfs3Target: TargetKeywordsReader[F] = target(sys.gwfs3Target)

    override def gwfs4Target: TargetKeywordsReader[F] = target(sys.gwfs4Target)

    override def gwfsTarget(v: VirtualGemsTelescope): TargetKeywordsReader[F] = target(sys.gemsTarget(v))

    override def gwfsMap: F[Map[VirtualGemsTelescope, GemsSource]] = for{
      s1 <- sys.g1MapName
      s2 <- sys.g2MapName
      s3 <- sys.g3MapName
      s4 <- sys.g4MapName
    } yield Map(
      VirtualGemsTelescope.G1 -> s1,
      VirtualGemsTelescope.G2 -> s2,
      VirtualGemsTelescope.G3 -> s3,
      VirtualGemsTelescope.G4 -> s4
    ).flattenOption

    override def m2UserFocusOffset: F[Double] = sys.m2UserFocusOffset.safeValOrDefault

    private def calcFrequency(t: Double): Double = if (t > 0.0) 1.0 / t else DefaultHeaderValue[Double].default

    override def pwfs1Freq: F[Double] = sys.pwfs1IntegrationTime.map(calcFrequency).safeValOrDefault

    override def pwfs2Freq: F[Double] = sys.pwfs2IntegrationTime.map(calcFrequency).safeValOrDefault

    override def oiwfsFreq: F[Double] = sys.oiwfsIntegrationTime.map(calcFrequency).safeValOrDefault

    override def carouselMode: F[String] = sys.carouselMode.safeValOrDefault

    override def gnirsInstPort: F[Int] = sys.gnirsPort.safeValOrDefault

    override def gpiInstPort: F[Int] = sys.gpiPort.safeValOrDefault

    override def niriInstPort: F[Int] = sys.niriPort.safeValOrDefault

    override def nifsInstPort: F[Int] = sys.nifsPort.safeValOrDefault

    override def gsaoiInstPort: F[Int] = sys.gsaoiPort.safeValOrDefault

    override def f2InstPort: F[Int] = sys.f2Port.safeValOrDefault

    override def crFollow: F[Option[CRFollow]] =
      sys.crFollow.map(CRFollow.fromInt.getOption)
      .handleError(_ => none)

    private def pOffsetOption: F[Option[Angle]] = (
      for {
        xoff <- OptionT(xOffsetOption)
        yoff <- OptionT(yOffsetOption)
        iaa  <- OptionT.liftF(sys.instrAA.map(Degrees(_)))
      } yield  -xoff * iaa.cos + yoff * iaa.sin
    ).value
     .handleError(_ => none)

    override def pOffset: F[Double] = Nested(pOffsetOption).map(_.toArcseconds).value.safeValOrDefault

    private def qOffsetOption: F[Option[Angle]] = (
      for {
        xoff <- OptionT(xOffsetOption)
        yoff <- OptionT(yOffsetOption)
        iaa  <- OptionT.liftF(sys.instrAA.map(Degrees(_)))
      } yield  -xoff * iaa.sin - yoff * iaa.cos
    ).value
     .handleError(_ => none)

    override def qOffset: F[Double] = Nested(qOffsetOption).map(_.toArcseconds).value.safeValOrDefault

    override def raOffset: F[Double] = (
      for {
        p   <- OptionT(pOffsetOption)
        q   <- OptionT(qOffsetOption)
        ipa <- OptionT.liftF(sys.instrPA.map(Degrees(_)))
      } yield p * ipa.cos + q * ipa.sin
    ).map(_.toArcseconds).value.safeValOrDefault

    override def decOffset: F[Double] = (
      for {
        p   <- OptionT(pOffsetOption)
        q   <- OptionT(qOffsetOption)
        ipa <- OptionT.liftF(sys.instrPA.map(Degrees(_)))
      } yield -p * ipa.sin + q * ipa.cos
    ).map(_.toArcseconds).value.safeValOrDefault
  }
}
