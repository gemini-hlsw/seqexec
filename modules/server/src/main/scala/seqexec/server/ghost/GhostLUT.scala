// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import scala.concurrent.duration._
import cats.syntax.all._
import edu.gemini.spModel.gemini.ghost.GhostBinning
import seqexec.model.Conditions
import seqexec.model.enum.ImageQuality
import seqexec.model.enum.CloudCover
import seqexec.model.enum.SkyBackground
import squants.time.Seconds
import squants.time.Milliseconds
import squants.time.Time
import java.time.{ Duration => JDuration }

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
      SVCameraTimes(19.5, 600.0, 600.0),
      SVCameraTimes(20.0, 600.0, 600.0)
    ).sortBy(_.gMag)

  // the List is never empty
  val SVMinimumTime = SVCameraTimesLUT.minBy(_.goodWeather)

  private def duration(sec: Int, milliSeconds: Int): JDuration =
    JDuration.ofSeconds(sec.toLong, milliSeconds.toLong * 1000000L)

  // TODO: Call directlyy the OCS code
  // Taken from OCS GhostCameras.scala
  object Red {
    val ReadoutTime: Map[(GhostBinning, ReadNoiseGain), JDuration] = {
      import GhostBinning._
      import ReadNoiseGain._
      Map(
        (ONE_BY_ONE, Slow)     -> duration(100, 675),
        (ONE_BY_ONE, Medium)   -> duration(58, 994),
        (ONE_BY_ONE, Fast)     -> duration(23, 520),
        (ONE_BY_TWO, Slow)     -> duration(51, 271),
        (ONE_BY_TWO, Medium)   -> duration(30, 230),
        (ONE_BY_TWO, Fast)     -> duration(12, 341),
        (ONE_BY_FOUR, Slow)    -> duration(26, 564),
        (ONE_BY_FOUR, Medium)  -> duration(15, 838),
        (ONE_BY_FOUR, Fast)    -> duration(6, 773),
        (ONE_BY_EIGHT, Slow)   -> duration(14, 198),
        (ONE_BY_EIGHT, Medium) -> duration(8, 686),
        (ONE_BY_EIGHT, Fast)   -> duration(3, 977),
        (TWO_BY_TWO, Slow)     -> duration(28, 364),
        (TWO_BY_TWO, Medium)   -> duration(17, 696),
        (TWO_BY_TWO, Fast)     -> duration(8, 577),
        (TWO_BY_FOUR, Slow)    -> duration(15, 146),
        (TWO_BY_FOUR, Medium)  -> duration(9, 638),
        (TWO_BY_FOUR, Fast)    -> duration(4, 929),
        (TWO_BY_EIGHT, Slow)   -> duration(8, 534),
        (TWO_BY_EIGHT, Medium) -> duration(5, 580),
        (TWO_BY_EIGHT, Fast)   -> duration(3, 578),
        (FOUR_BY_FOUR, Slow)   -> duration(9, 536),
        (FOUR_BY_FOUR, Medium) -> duration(6, 581),
        (FOUR_BY_FOUR, Fast)   -> duration(4, 770)
      )
    }
  }

  object Blue {

    val ReadoutTime: Map[(GhostBinning, ReadNoiseGain), JDuration] = {
      import GhostBinning._
      import ReadNoiseGain._
      Map(
        (ONE_BY_ONE, Slow)     -> duration(45, 957),
        (ONE_BY_ONE, Medium)   -> duration(27, 118),
        (ONE_BY_ONE, Fast)     -> duration(11, 78),
        (ONE_BY_TWO, Slow)     -> duration(23, 808),
        (ONE_BY_TWO, Medium)   -> duration(14, 237),
        (ONE_BY_TWO, Fast)     -> duration(6, 72),
        (ONE_BY_FOUR, Slow)    -> duration(12, 741),
        (ONE_BY_FOUR, Medium)  -> duration(7, 784),
        (ONE_BY_FOUR, Fast)    -> duration(3, 575),
        (ONE_BY_EIGHT, Slow)   -> duration(7, 229),
        (ONE_BY_EIGHT, Medium) -> duration(4, 574),
        (ONE_BY_EIGHT, Fast)   -> duration(3, 75),
        (TWO_BY_TWO, Slow)     -> duration(13, 644),
        (TWO_BY_TWO, Medium)   -> duration(8, 633),
        (TWO_BY_TWO, Fast)     -> duration(4, 425),
        (TWO_BY_FOUR, Slow)    -> duration(7, 68),
        (TWO_BY_FOUR, Medium)  -> duration(5, 24),
        (TWO_BY_FOUR, Fast)    -> duration(3, 71),
        (TWO_BY_EIGHT, Slow)   -> duration(4, 722),
        (TWO_BY_EIGHT, Medium) -> duration(3, 223),
        (TWO_BY_EIGHT, Fast)   -> duration(3, 42),
        (FOUR_BY_FOUR, Slow)   -> duration(5, 226),
        (FOUR_BY_FOUR, Medium) -> duration(3, 722),
        (FOUR_BY_FOUR, Fast)   -> duration(3, 44)
      )
    }
  }

  // Readout time to fallback
  val fallbackReadoutTimeRed: JDuration = Red.ReadoutTime.map(_._2).max

  val fallbackReadoutTimeBlue: JDuration = Blue.ReadoutTime.map(_._2).max

  // REL-4239
  def totalObserveTime(blueChannel: ChannelConfig, redChannel: ChannelConfig): Time = {
    val blueKey   =
      (blueChannel.binning, blueChannel.readMode)
    val blue      = Blue.ReadoutTime.getOrElse(blueKey, fallbackReadoutTimeBlue)
    val redKey    =
      (redChannel.binning, redChannel.readMode)
    val red       = Red.ReadoutTime.getOrElse(redKey, fallbackReadoutTimeRed)
    val blueTotal =
      blueChannel.count.toLong * (blueChannel.exposure + Duration.fromNanos(
        blue.toNanos
      ))
    val redTotal  =
      redChannel.count.toLong * (redChannel.exposure + Duration.fromNanos(red.toNanos))

    Milliseconds(blueTotal.max(redTotal).toMillis)
  }

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

  val svReadoutTime = Seconds(2.0)

  // REL-4270
  def svCameraRepeats(
    conditions: Conditions,
    mag:        Option[Double],
    blueConfig: ChannelConfig,
    redConfig:  ChannelConfig
  ): Int = {
    val total  = totalObserveTime(blueConfig, redConfig)
    val svTime = Seconds(svCameraTime(conditions, mag))
    (total / (svTime + svReadoutTime)).floor.toInt
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
  val FlatSVTime = 0.1.seconds
  val ArcSVTime  = 300.seconds

  def svCalibExposureTime(obsType: String) = {
    val isBias: Boolean = imageTypeConf(obsType).equalsIgnoreCase("BIAS")
    val isFlat: Boolean = imageTypeConf(obsType).equalsIgnoreCase("FLAT")
    val isArc: Boolean  = imageTypeConf(obsType).equalsIgnoreCase("ARC")

    if (isBias) BiasSVTime else if (isFlat) FlatSVTime else if (isArc) ArcSVTime else 0.seconds
  }

}

object GhostLUT extends GhostLUT
