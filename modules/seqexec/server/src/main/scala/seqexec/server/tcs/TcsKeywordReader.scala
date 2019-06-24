// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats._
import cats.effect.LiftIO
import cats.effect.Sync
import cats.data.OptionT
import cats.data.Nested
import cats.implicits._
import gsp.math.syntax.string._
import java.time.LocalDate
import java.time.format.DateTimeFormatter
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

  def airMass: F[Double]

  def startAirMass: F[Double]

  def endAirMass: F[Double]

  def carouselMode: F[String]

  def m2UserFocusOffset: F[Double]

  def parallacticAngle: F[Option[Angle]]

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

    override def parallacticAngle: F[Option[Angle]] = Arcseconds(0.0).some.pure[F]

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
  def apply[F[_]: Sync: LiftIO]: TcsKeywordsReader[F] = new TcsKeywordsReader[F] {
    override def hourAngle: F[String] = TcsEpics.instance.hourAngle.safeValOrDefault.to[F]

    override def localTime: F[String] = TcsEpics.instance.localTime.safeValOrDefault.to[F]

    override def trackingFrame: F[String] = TcsEpics.instance.trackingFrame.safeValOrDefault.to[F]

    override def trackingEpoch: F[Double] = TcsEpics.instance.trackingEpoch.safeValOrDefault.to[F]

    private def translateEpoch(v: Option[String]): Option[Double] =
      v.filter(_.nonEmpty).flatMap(_.drop(1).parseDoubleOption)

    override def trackingEquinox: F[Double] = TcsEpics.instance.trackingEquinox.map(translateEpoch).safeValOrDefault.to[F]

    override def trackingDec: F[Double] = TcsEpics.instance.trackingDec.safeValOrDefault.to[F]

    override def trackingRA: F[Double] = TcsEpics.instance.trackingRA.safeValOrDefault.to[F]

    override def elevation: F[Double] = TcsEpics.instance.elevation.safeValOrDefault.to[F]

    override def azimuth: F[Double] = TcsEpics.instance.azimuth.safeValOrDefault.to[F]

    override def crPositionAngle: F[Double] = TcsEpics.instance.crPositionAngle.safeValOrDefault.to[F]

    override def ut: F[String] = TcsEpics.instance.ut.safeValOrDefault.to[F]

    override def date: F[String] = TcsEpics.instance.date.safeValOrDefault.to[F]

    override def m2Baffle: F[String] = TcsEpics.instance.m2Baffle.safeValOrDefault.to[F]

    override def m2CentralBaffle: F[String] = TcsEpics.instance.m2CentralBaffle.safeValOrDefault.to[F]

    override def st: F[String] = TcsEpics.instance.st.safeValOrDefault.to[F]

    override def sfRotation: F[Double] = TcsEpics.instance.sfRotation.safeValOrDefault.to[F]

    override def sfTilt: F[Double] = TcsEpics.instance.sfTilt.safeValOrDefault.to[F]

    override def sfLinear: F[Double] = TcsEpics.instance.sfLinear.safeValOrDefault.to[F]

    override def instrumentPA: F[Double] = TcsEpics.instance.instrPA.safeValOrDefault.to[F]

    override def gmosInstPort: F[Int] = TcsEpics.instance.gmosPort.safeValOrDefault.to[F]

    private val xoffIndex = 6
    private val yoffIndex = 7

    private def xOffsetOption: F[Option[Angle]] =
      TcsEpics.instance.targetA
        .map(_.flatMap(v => v.lift(xoffIndex).map(x => Millimeters(x) * FOCAL_PLANE_SCALE)))
        .handleError(_ => none)
        .to[F]

    override def xOffset: F[Double] =
      Nested(xOffsetOption).map(_.toArcseconds).value.safeValOrDefault

    private def yOffsetOption: F[Option[Angle]] =
      TcsEpics.instance.targetA
        .map(_.flatMap(v => v.lift(yoffIndex).map(x => Millimeters(x) * FOCAL_PLANE_SCALE)))
        .handleError(_ => none)
        .to[F]

    override def yOffset: F[Double] =
      Nested(yOffsetOption).map(_.toArcseconds).value.safeValOrDefault

    private val raoffIndex = 2
    private val decoffIndex = 3

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

      TcsEpics.instance.targetA.map(_.flatMap(v =>
        Apply[Option].ap2(Option(raOffset _))(v.lift(raoffIndex).map(Radians(_)),v.lift(decoffIndex).map(Radians(_)))))
        .map(_.map(_.toArcseconds))
        .safeValOrDefault.to[F]
    }

    override def trackingDecOffset: F[Double] =
      TcsEpics.instance.targetA
        .map(_.flatMap(v => v.lift(decoffIndex).map(Radians(_).toArcseconds)))
        .safeValOrDefault.to[F]

    override def instrumentAA: F[Double] = TcsEpics.instance.instrAA.safeValOrDefault.to[F]

    override def aoFoldName: F[String] = TcsEpics.instance.aoFoldPosition.safeValOrDefault.to[F]

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

    override def sourceATarget: TargetKeywordsReader[F] = target(TcsEpics.instance.sourceATarget.to[F])

    override def airMass: F[Double] =  TcsEpics.instance.airmass.safeValOrDefault.to[F]

    override def startAirMass: F[Double] = TcsEpics.instance.airmassStart.safeValOrDefault.to[F]

    override def endAirMass: F[Double] = TcsEpics.instance.airmassEnd.safeValOrDefault.to[F]

    override def parallacticAngle: F[Option[Angle]] =
      Nested(TcsEpics.instance.parallacticAngle)
        .map(normalizeSignedAngle).value
        .handleError(_ => none)
        .to[F]

    override def pwfs1Target: TargetKeywordsReader[F] = target(TcsEpics.instance.pwfs1Target.to[F])

    override def pwfs2Target: TargetKeywordsReader[F] = target(TcsEpics.instance.pwfs2Target.to[F])

    override def oiwfsTarget: TargetKeywordsReader[F] = target(TcsEpics.instance.oiwfsTarget.to[F])

    override def aowfsTarget: TargetKeywordsReader[F] = target(TcsEpics.instance.pwfs2Target.to[F])

    override def gwfs1Target: TargetKeywordsReader[F] = target(TcsEpics.instance.gwfs1Target.to[F])

    override def gwfs2Target: TargetKeywordsReader[F] = target(TcsEpics.instance.gwfs2Target.to[F])

    override def gwfs3Target: TargetKeywordsReader[F] = target(TcsEpics.instance.gwfs3Target.to[F])

    override def gwfs4Target: TargetKeywordsReader[F] = target(TcsEpics.instance.gwfs4Target.to[F])

    override def m2UserFocusOffset: F[Double] = TcsEpics.instance.m2UserFocusOffset.safeValOrDefault.to[F]

    private def calcFrequency(t: Double): Double = if (t > 0.0) 1.0 / t else DefaultHeaderValue[Double].default

    override def pwfs1Freq: F[Double] =
      Nested(TcsEpics.instance.pwfs1IntegrationTime).map(calcFrequency).value.safeValOrDefault.to[F]

    override def pwfs2Freq: F[Double] =
      Nested(TcsEpics.instance.pwfs2IntegrationTime).map(calcFrequency).value.safeValOrDefault.to[F]

    override def oiwfsFreq: F[Double] =
      Nested(TcsEpics.instance.oiwfsIntegrationTime).map(calcFrequency).value.safeValOrDefault.to[F]

    override def carouselMode: F[String] = TcsEpics.instance.carouselMode.safeValOrDefault.to[F]

    override def gnirsInstPort: F[Int] = TcsEpics.instance.gnirsPort.safeValOrDefault.to[F]

    override def gpiInstPort: F[Int] = TcsEpics.instance.gpiPort.safeValOrDefault.to[F]

    override def niriInstPort: F[Int] = TcsEpics.instance.niriPort.safeValOrDefault.to[F]

    override def nifsInstPort: F[Int] = TcsEpics.instance.nifsPort.safeValOrDefault.to[F]

    override def gsaoiInstPort: F[Int] = TcsEpics.instance.gsaoiPort.safeValOrDefault.to[F]

    override def f2InstPort: F[Int] = TcsEpics.instance.f2Port.safeValOrDefault.to[F]

    override def crFollow: F[Option[CRFollow]] =
      TcsEpics.instance.crFollow.map(_.flatMap(CRFollow.fromInt.getOption))
      .handleError(_ => none)
      .to[F]

    private def pOffsetOption: F[Option[Angle]] = (
      for {
        xoff <- OptionT(xOffsetOption)
        yoff <- OptionT(yOffsetOption)
        iaa  <- OptionT(TcsEpics.instance.instrAA.to[F]).map(Degrees(_))
      } yield  -xoff * iaa.cos + yoff * iaa.sin
    ).value
     .handleError(_ => none)

    override def pOffset: F[Double] = Nested(pOffsetOption).map(_.toArcseconds).value.safeValOrDefault

    private def qOffsetOption: F[Option[Angle]] = (
      for {
        xoff <- OptionT(xOffsetOption)
        yoff <- OptionT(yOffsetOption)
        iaa  <- OptionT(TcsEpics.instance.instrAA.to[F]).map(Degrees(_))
      } yield  -xoff * iaa.sin - yoff * iaa.cos
    ).value
     .handleError(_ => none)

    override def qOffset: F[Double] = Nested(qOffsetOption).map(_.toArcseconds).value.safeValOrDefault

    override def raOffset: F[Double] = (
      for {
        p <- OptionT(pOffsetOption)
        q <- OptionT(qOffsetOption)
        ipa <- OptionT(TcsEpics.instance.instrPA.to[F]).map(Degrees(_))
      } yield p * ipa.cos + q * ipa.sin
    ).map(_.toArcseconds).value.safeValOrDefault

    override def decOffset: F[Double] = (
      for {
        p <- OptionT(pOffsetOption)
        q <- OptionT(qOffsetOption)
        ipa <- OptionT(TcsEpics.instance.instrPA.to[F]).map(Degrees(_))
      } yield -p * ipa.sin + q * ipa.cos
    ).map(_.toArcseconds).value.safeValOrDefault
  }
}
