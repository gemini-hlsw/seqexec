// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import cats.Applicative
import cats.data.Nested
import cats.effect.Sync
import cats.effect.LiftIO
import cats.implicits._
import seqexec.server.keywords._

trait NiriKeywordReader[F[_]] {
  def arrayId: F[String]
  def arrayType: F[String]
  def camera: F[String]
  def coadds: F[Int]
  def exposureTime: F[Double]
  def filter1: F[String]
  def filter2: F[String]
  def filter3: F[String]
  def focusName: F[String]
  def focusPosition: F[Double]
  def focalPlaneMask: F[String]
  def beamSplitter: F[String]
  def windowCover: F[String]
  def framesPerCycle: F[Int]
  def headerTiming: F[String]
  def lnrs: F[Int]
  def mode: F[String]
  def numberDigitalAverage: F[Int]
  def pupilViewer: F[String]
  def detectorTemperature: F[Double]
  def mountTemperature: F[Double]
  def µcodeName: F[String]
  def µcodeType: F[String]
  def cl1VoltageDD: F[Double]
  def cl2VoltageDD: F[Double]
  def ucVoltage: F[Double]
  def detectorVoltage: F[Double]
  def cl1VoltageGG: F[Double]
  def cl2VoltageGG: F[Double]
  def setVoltage: F[Double]
  def observationEpoch: F[Double]
}

// This could be NiriKeywordReader[Id] but it requires changes upstream
object NiriKeywordReaderDummy {
  def apply[F[_]: Applicative]: NiriKeywordReader[F] = new NiriKeywordReader[F] {
    override def arrayId: F[String] = strDefault[F]
    override def arrayType: F[String] = strDefault[F]
    override def camera: F[String] = strDefault[F]
    override def coadds: F[Int] = intDefault[F]
    override def exposureTime: F[Double] = doubleDefault[F]
    override def filter1: F[String] = strDefault[F]
    override def filter2: F[String] = strDefault[F]
    override def filter3: F[String] = strDefault[F]
    override def focusName: F[String] = strDefault[F]
    override def focusPosition: F[Double] = doubleDefault[F]
    override def focalPlaneMask: F[String] = strDefault[F]
    override def beamSplitter: F[String] = strDefault[F]
    override def windowCover: F[String] = strDefault[F]
    override def framesPerCycle: F[Int] = intDefault[F]
    override def headerTiming: F[String] = strDefault[F]
    override def lnrs: F[Int] = intDefault[F]
    override def mode: F[String] = strDefault[F]
    override def numberDigitalAverage: F[Int] = intDefault[F]
    override def pupilViewer: F[String] = strDefault[F]
    override def detectorTemperature: F[Double] = doubleDefault[F]
    override def mountTemperature: F[Double] = doubleDefault[F]
    override def µcodeName: F[String] = strDefault[F]
    override def µcodeType: F[String] = strDefault[F]
    override def cl1VoltageDD: F[Double] = doubleDefault[F]
    override def cl2VoltageDD: F[Double] = doubleDefault[F]
    override def ucVoltage: F[Double] = doubleDefault[F]
    override def detectorVoltage: F[Double] = doubleDefault[F]
    override def cl1VoltageGG: F[Double] = doubleDefault[F]
    override def cl2VoltageGG: F[Double] = doubleDefault[F]
    override def setVoltage: F[Double] = doubleDefault[F]
    override def observationEpoch: F[Double] = doubleDefault[F]
  }
}

object NiriKeywordReaderEpics {
  def apply[F[_]: Sync: LiftIO]: NiriKeywordReader[F] = new NiriKeywordReader[F] {
    val sys = NiriEpics.instance
    override def arrayId: F[String] = sys.arrayId.safeValOrDefault.to[F]
    override def arrayType: F[String] = sys.arrayType.safeValOrDefault.to[F]
    override def camera: F[String] = sys.camera.safeValOrDefault.to[F]
    override def coadds: F[Int] = sys.coadds.safeValOrDefault.to[F]
    override def exposureTime: F[Double] =
      (for {
        it <- sys.integrationTime
        mi <- sys.minIntegration
      } yield (it, mi).mapN(_ + _)).safeValOrDefault.to[F]
    override def filter1: F[String] = sys.filter1.safeValOrDefault.to[F]
    override def filter2: F[String] = sys.filter2.safeValOrDefault.to[F]
    override def filter3: F[String] = sys.filter3.safeValOrDefault.to[F]
    override def focusName: F[String] = sys.focus.safeValOrDefault.to[F]
    override def focusPosition: F[Double] = sys.focusPosition.safeValOrDefault.to[F]
    override def focalPlaneMask: F[String] = sys.mask.safeValOrDefault.to[F]
    override def beamSplitter: F[String] = sys.beamSplitter.safeValOrDefault.to[F]
    override def windowCover: F[String] = sys.windowCover.safeValOrDefault.to[F]
    override def framesPerCycle: F[Int] = sys.framesPerCycle.safeValOrDefault.to[F]
    override def headerTiming: F[String] = Nested(sys.hdrTiming).map {
      case 1 => "BEFORE"
      case 2 => "AFTER"
      case 3 => "BOTH"
      case _ => "INDEF"
    }.value.safeValOrDefault.to[F]
    override def lnrs: F[Int] = sys.lnrs.safeValOrDefault.to[F]
    override def mode: F[String] = Nested(sys.mode).map {
      case 0 => "STARE"
      case 1 => "SEP"
      case 2 => "CHOP"
      case 3 => "CHOP2"
      case 4 => "TEST"
      case _ => "INDEF"
    }.value.safeValOrDefault.to[F]
    override def numberDigitalAverage: F[Int] = sys.digitalAverageCount.safeValOrDefault.to[F]
    override def pupilViewer: F[String] = sys.pupilViewer.safeValOrDefault.to[F]
    override def detectorTemperature: F[Double] = sys.detectorTemp.safeValOrDefault.to[F]
    override def mountTemperature: F[Double] = sys.mountTemp.safeValOrDefault.to[F]
    override def µcodeName: F[String] = sys.µcodeName.safeValOrDefault.to[F]
    override def µcodeType: F[String] = Nested(sys.µcodeType).map {
      case 1 => "RRD"
      case 2 => "RDD"
      case 3 => "RD"
      case 4 => "SRB"
      case _ => "INDEF"
    }.value.safeValOrDefault.to[F]
    override def cl1VoltageDD: F[Double] = sys.vddCl1.safeValOrDefault.to[F]
    override def cl2VoltageDD: F[Double] = sys.vddCl2.safeValOrDefault.to[F]
    override def ucVoltage: F[Double] = sys.vddUc.safeValOrDefault.to[F]
    override def detectorVoltage: F[Double] = sys.detectorVDetBias.safeValOrDefault.to[F]
    override def cl1VoltageGG: F[Double] = sys.vggCl1.safeValOrDefault.to[F]
    override def cl2VoltageGG: F[Double] = sys.vggCl2.safeValOrDefault.to[F]
    override def setVoltage: F[Double] = sys.detectorVSetBias.safeValOrDefault.to[F]
    override def observationEpoch: F[Double] = sys.obsEpoch.safeValOrDefault.to[F]
  }
}
