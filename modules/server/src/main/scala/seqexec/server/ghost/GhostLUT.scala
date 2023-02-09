// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import scala.concurrent.duration._
import cats.syntax.all._
import edu.gemini.spModel.gemini.ghost.GhostBinning
import seqexec.model.Conditions
import seqexec.model.enum.ImageQuality
import seqexec.model.enum.CloudCover
import seqexec.model.enum.SkyBackground

final case class GuideCameraTimes(gMag: Double, poorWeather: Double, goodWeather: Double)
final case class SVCameraTimes(gMag: Double, poorWeather: Double, goodWeather: Double)
final case class ReadoutTimes(
  mode:     ReadNoiseGain,
  binning:  GhostBinning,
  readRed:  Duration,
  readBlue: Duration
)

// GHOST Lookup tables
trait GhostLUT {
  val GuideCameraTimesLUT =
    List(
      GuideCameraTimes(03.0, 00.1, 00.1),
      GuideCameraTimes(03.5, 00.1, 00.1),
      GuideCameraTimes(04.0, 00.2, 00.1),
      GuideCameraTimes(04.5, 00.2, 00.1),
      GuideCameraTimes(05.0, 00.2, 00.1),
      GuideCameraTimes(05.5, 00.3, 00.2),
      GuideCameraTimes(06.0, 00.3, 00.2),
      GuideCameraTimes(06.5, 00.4, 00.2),
      GuideCameraTimes(07.0, 00.5, 00.2),
      GuideCameraTimes(07.5, 00.6, 00.3),
      GuideCameraTimes(08.0, 00.7, 00.3),
      GuideCameraTimes(08.5, 00.8, 00.4),
      GuideCameraTimes(09.0, 00.9, 00.5),
      GuideCameraTimes(09.5, 01.0, 00.6),
      GuideCameraTimes(10.0, 01.0, 00.7),
      GuideCameraTimes(10.5, 02.0, 00.8),
      GuideCameraTimes(11.0, 02.0, 00.9),
      GuideCameraTimes(11.5, 02.0, 01.0),
      GuideCameraTimes(12.0, 03.0, 01.0),
      GuideCameraTimes(12.5, 03.0, 02.0),
      GuideCameraTimes(13.0, 04.0, 02.0),
      GuideCameraTimes(13.5, 05.0, 02.0),
      GuideCameraTimes(14.0, 06.0, 02.0),
      GuideCameraTimes(14.5, 07.0, 03.0),
      GuideCameraTimes(15.0, 08.0, 03.0),
      GuideCameraTimes(15.5, 10.0, 04.0),
      GuideCameraTimes(16.0, 11.0, 05.0),
      GuideCameraTimes(16.5, 14.0, 06.0),
      GuideCameraTimes(17.0, 16.0, 06.0),
      GuideCameraTimes(17.5, 19.0, 08.0),
      GuideCameraTimes(18.0, 23.0, 09.0),
      GuideCameraTimes(18.5, 28.0, 11.0),
      GuideCameraTimes(19.0, 30.0, 12.0),
      GuideCameraTimes(19.5, 40.0, 15.0),
      GuideCameraTimes(20.0, 50.0, 17.0)
    ).sortBy(_.gMag)

  // the List is never empty
  // Value for mag 17 good weather, i.e. 6 secs
  val AGMinimumTime: Double = GuideCameraTimesLUT.find(_.gMag == 17.0).map(_.goodWeather).get

  val SVCameraTimesLUT =
    List(
      SVCameraTimes(3.0, 0.2, 0.2),
      SVCameraTimes(3.5, 0.2, 0.2),
      SVCameraTimes(4.0, 0.3, 0.2),
      SVCameraTimes(4.5, 0.3, 0.3),
      SVCameraTimes(5.0, 0.4, 0.3),
      SVCameraTimes(5.5, 0.6, 0.4),
      SVCameraTimes(6.0, 0.7, 0.6),
      SVCameraTimes(6.5, 1.0, 0.7),
      SVCameraTimes(7.0, 1.0, 1.0),
      SVCameraTimes(7.5, 2.0, 1.0),
      SVCameraTimes(8.0, 2.0, 2.0),
      SVCameraTimes(8.5, 3.0, 2.0),
      SVCameraTimes(9.0, 4.0, 3.0),
      SVCameraTimes(9.5, 5.0, 3.0),
      SVCameraTimes(10.0, 6.0, 4.0),
      SVCameraTimes(10.5, 8.0, 6.0),
      SVCameraTimes(11.0, 10.0, 7.0),
      SVCameraTimes(11.5, 14.0, 9.0),
      SVCameraTimes(12.0, 18.0, 12.0),
      SVCameraTimes(12.5, 23.0, 15.0),
      SVCameraTimes(13.0, 30.0, 20.0),
      SVCameraTimes(13.5, 40.0, 25.0),
      SVCameraTimes(14.0, 50.0, 30.0),
      SVCameraTimes(14.5, 70.0, 40.0),
      SVCameraTimes(15.0, 90.0, 50.0),
      SVCameraTimes(15.5, 110.0, 70.0),
      SVCameraTimes(16.0, 150.0, 90.0),
      SVCameraTimes(16.5, 190.0, 110.0),
      SVCameraTimes(17.0, 250.0, 150.0),
      SVCameraTimes(17.5, 300.0, 190.0),
      SVCameraTimes(18.0, 300.0, 240.0),
      SVCameraTimes(18.5, 300.0, 300.0),
      SVCameraTimes(19.0, 300.0, 300.0),
      SVCameraTimes(19.5, 300.0, 300.0),
      SVCameraTimes(20.0, 300.0, 300.0)
    ).sortBy(_.gMag)

  // the List is never empty
  val SVMinimumTime = SVCameraTimesLUT.minBy(_.goodWeather)

  val ReadoutTimesLUT = List(
    ReadoutTimes(ReadNoiseGain.Slow, GhostBinning.ONE_BY_ONE, 97.4.seconds, 45.6.seconds),
    ReadoutTimes(ReadNoiseGain.Slow, GhostBinning.ONE_BY_TWO, 49.6.seconds, 24.8.seconds),
    ReadoutTimes(ReadNoiseGain.Slow, GhostBinning.ONE_BY_FOUR, 25.7.seconds, 14.4.seconds),
    ReadoutTimes(ReadNoiseGain.Slow, GhostBinning.ONE_BY_EIGHT, 13.8.seconds, 9.1.seconds),
    ReadoutTimes(ReadNoiseGain.Slow, GhostBinning.TWO_BY_TWO, 27.5.seconds, 15.4.seconds),
    ReadoutTimes(ReadNoiseGain.Slow, GhostBinning.TWO_BY_FOUR, 14.7.seconds, 9.8.seconds),
    ReadoutTimes(ReadNoiseGain.Slow, GhostBinning.TWO_BY_EIGHT, 8.4.seconds, 7.0.seconds),
    ReadoutTimes(ReadNoiseGain.Slow, GhostBinning.FOUR_BY_FOUR, 9.5.seconds, 7.9.seconds),
    ReadoutTimes(ReadNoiseGain.Medium, GhostBinning.ONE_BY_ONE, 50.1.seconds, 24.6.seconds),
    ReadoutTimes(ReadNoiseGain.Medium, GhostBinning.ONE_BY_TWO, 26.1.seconds, 14.3.seconds),
    ReadoutTimes(ReadNoiseGain.Medium, GhostBinning.ONE_BY_FOUR, 13.9.seconds, 9.1.seconds),
    ReadoutTimes(ReadNoiseGain.Medium, GhostBinning.ONE_BY_EIGHT, 7.9.seconds, 6.5.seconds),
    ReadoutTimes(ReadNoiseGain.Medium, GhostBinning.TWO_BY_TWO, 15.7.seconds, 10.1.seconds),
    ReadoutTimes(ReadNoiseGain.Medium, GhostBinning.TWO_BY_FOUR, 8.8.seconds, 7.2.seconds),
    ReadoutTimes(ReadNoiseGain.Medium, GhostBinning.TWO_BY_EIGHT, 5.4.seconds, 5.6.seconds),
    ReadoutTimes(ReadNoiseGain.Medium, GhostBinning.FOUR_BY_FOUR, 6.5.seconds, 6.5.seconds),
    ReadoutTimes(ReadNoiseGain.Fast, GhostBinning.ONE_BY_ONE, 21.7.seconds, 12.0.seconds),
    ReadoutTimes(ReadNoiseGain.Fast, GhostBinning.ONE_BY_TWO, 11.7.seconds, 7.9.seconds),
    ReadoutTimes(ReadNoiseGain.Fast, GhostBinning.ONE_BY_FOUR, 6.8.seconds, 5.9.seconds),
    ReadoutTimes(ReadNoiseGain.Fast, GhostBinning.ONE_BY_EIGHT, 4.3.seconds, 4.9.seconds),
    ReadoutTimes(ReadNoiseGain.Fast, GhostBinning.TWO_BY_TWO, 8.6.seconds, 6.9.seconds),
    ReadoutTimes(ReadNoiseGain.Fast, GhostBinning.TWO_BY_FOUR, 5.2.seconds, 5.6.seconds),
    ReadoutTimes(ReadNoiseGain.Fast, GhostBinning.TWO_BY_EIGHT, 3.6.seconds, 4.9.seconds),
    ReadoutTimes(ReadNoiseGain.Fast, GhostBinning.FOUR_BY_FOUR, 4.7.seconds, 5.8.seconds)
  )

  def isPoorWeather(conditions: Conditions) =
    conditions.sb >= SkyBackground.Percent80 || conditions.cc >= CloudCover.Percent80 || conditions.iq === ImageQuality.Any || conditions.sb === SkyBackground.Unknown || conditions.cc === CloudCover.Unknown || conditions.iq === ImageQuality.Unknown

  def svCameraTime(conditions: Conditions, mag: Option[Double]): Double = {
    val times = mag
      .flatMap(mag =>
        SVCameraTimesLUT
          .find(_.gMag > mag)
      )
      .getOrElse(SVMinimumTime)
    if (isPoorWeather(conditions)) times.poorWeather else times.goodWeather
  }

  def agCameraTime(conditions: Conditions, mag: Option[Double]): Double = {
    val times = mag
      .flatMap(mag =>
        GuideCameraTimesLUT
          .find(_.gMag > mag)
      )
    (if (isPoorWeather(conditions)) times.map(_.poorWeather) else times.map(_.goodWeather))
      .getOrElse(AGMinimumTime)
  }

  def imageTypeConf(obsType: String) = obsType.toLowerCase match {
    case "bias" => "BIAS"
    case "flat" => "FLAT"
    case "dark" => "DARK"
    case "arc"  => "ARC"
    case _      => "OBJECT"
  }

  def isScience(obsType: String): Boolean = obsType.equalsIgnoreCase("object")

  val BiasSVTime = 0.seconds
  val FlatSVTime = 0.3.seconds
  val ArcSVTime  = 300.seconds

  def svCalibExposureTime(obsType: String) = {
    val isBias: Boolean = imageTypeConf(obsType).equalsIgnoreCase("BIAS")
    val isFlat: Boolean = imageTypeConf(obsType).equalsIgnoreCase("FLAT")
    val isArc: Boolean  = imageTypeConf(obsType).equalsIgnoreCase("ARC")
    // val isDark: Boolean = imageTypeConf(obsType).equalsIgnoreCase("DARK")

    if (isBias) BiasSVTime else if (isFlat) FlatSVTime else if (isArc) ArcSVTime else 0.seconds
  }

}

object GhostLUT extends GhostLUT
