// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import cats.effect.IO
import cats.implicits._
import seqexec.server.SeqAction
import seqexec.server.SeqActionF
import seqexec.server.keywords._

trait NiriKeywordReader[F[_]] {
  def arrayId: SeqActionF[F, String]
  def arrayType: SeqActionF[F, String]
  def camera: SeqActionF[F, String]
  def coadds: SeqActionF[F, Int]
  def exposureTime: SeqActionF[F, Double]
  def filter1: SeqActionF[F, String]
  def filter2: SeqActionF[F, String]
  def filter3: SeqActionF[F, String]
  def focusName: SeqActionF[F, String]
  def focusPosition: SeqActionF[F, Double]
  def focalPlaneMask: SeqActionF[F, String]
  def beamSplitter: SeqActionF[F, String]
  def windowCover: SeqActionF[F, String]
  def framesPerCycle: SeqActionF[F, Int]
  def headerTiming: SeqActionF[F, String]
  def lnrs: SeqActionF[F, Int]
  def mode: SeqActionF[F, String]
  def numberDigitalAverage: SeqActionF[F, Int]
  def pupilViewer: SeqActionF[F, String]
  def detectorTemperature: SeqActionF[F, Double]
  def mountTemperature: SeqActionF[F, Double]
  def µcodeName: SeqActionF[F, String]
  def µcodeType: SeqActionF[F, String]
  def cl1VoltageDD: SeqActionF[F, Double]
  def cl2VoltageDD: SeqActionF[F, Double]
  def ucVoltage: SeqActionF[F, Double]
  def detectorVoltage: SeqActionF[F, Double]
  def cl1VoltageGG: SeqActionF[F, Double]
  def cl2VoltageGG: SeqActionF[F, Double]
  def setVoltage: SeqActionF[F, Double]
  def observationEpoch: SeqActionF[F, Double]
}

// This could be NiriKeywordReader[Id] but it requires changes upstream
object NiriKeywordReaderDummy extends NiriKeywordReader[IO] {
  override def arrayId: SeqAction[String] = SeqAction(StrDefault)
  override def arrayType: SeqAction[String] = SeqAction(StrDefault)
  override def camera: SeqAction[String] = SeqAction(StrDefault)
  override def coadds: SeqAction[Int] = SeqAction(IntDefault)
  override def exposureTime: SeqAction[Double] = SeqAction(DoubleDefault)
  override def filter1: SeqAction[String] = SeqAction(StrDefault)
  override def filter2: SeqAction[String] = SeqAction(StrDefault)
  override def filter3: SeqAction[String] = SeqAction(StrDefault)
  override def focusName: SeqAction[String] = SeqAction(StrDefault)
  override def focusPosition: SeqAction[Double] = SeqAction(DoubleDefault)
  override def focalPlaneMask: SeqAction[String] = SeqAction(StrDefault)
  override def beamSplitter: SeqAction[String] = SeqAction(StrDefault)
  override def windowCover: SeqAction[String] = SeqAction(StrDefault)
  override def framesPerCycle: SeqAction[Int] = SeqAction(IntDefault)
  override def headerTiming: SeqAction[String] = SeqAction(StrDefault)
  override def lnrs: SeqAction[Int] = SeqAction(IntDefault)
  override def mode: SeqAction[String] = SeqAction(StrDefault)
  override def numberDigitalAverage: SeqAction[Int] = SeqAction(IntDefault)
  override def pupilViewer: SeqAction[String] = SeqAction(StrDefault)
  override def detectorTemperature: SeqAction[Double] = SeqAction(DoubleDefault)
  override def mountTemperature: SeqAction[Double] = SeqAction(DoubleDefault)
  override def µcodeName: SeqAction[String] = SeqAction(StrDefault)
  override def µcodeType: SeqAction[String] = SeqAction(StrDefault)
  override def cl1VoltageDD: SeqAction[Double] = SeqAction(DoubleDefault)
  override def cl2VoltageDD: SeqAction[Double] = SeqAction(DoubleDefault)
  override def ucVoltage: SeqAction[Double] = SeqAction(DoubleDefault)
  override def detectorVoltage: SeqAction[Double] = SeqAction(DoubleDefault)
  override def cl1VoltageGG: SeqAction[Double] = SeqAction(DoubleDefault)
  override def cl2VoltageGG: SeqAction[Double] = SeqAction(DoubleDefault)
  override def setVoltage: SeqAction[Double] = SeqAction(DoubleDefault)
  override def observationEpoch: SeqAction[Double] = SeqAction(DoubleDefault)
}

object NiriKeywordReaderImpl extends NiriKeywordReader[IO] {
  val sys = NiriEpics.instance
  override def arrayId: SeqAction[String] = SeqActionF.embed(sys.arrayId.safeValOrDefault)
  override def arrayType: SeqAction[String] = SeqActionF.embed(sys.arrayType.safeValOrDefault)
  override def camera: SeqAction[String] = SeqActionF.embed(sys.camera.safeValOrDefault)
  override def coadds: SeqAction[Int] = SeqActionF.embed(sys.coadds.safeValOrDefault)
  override def exposureTime: SeqAction[Double] = SeqActionF.embed {
    (for {
      it <- sys.integrationTime
      mi <- sys.minIntegration
    } yield (it, mi).mapN(_ + _)).safeValOrDefault
  }
  override def filter1: SeqAction[String] = SeqActionF.embed(sys.filter1.safeValOrDefault)
  override def filter2: SeqAction[String] = SeqActionF.embed(sys.filter2.safeValOrDefault)
  override def filter3: SeqAction[String] = SeqActionF.embed(sys.filter3.safeValOrDefault)
  override def focusName: SeqAction[String] = SeqActionF.embed(sys.focus.safeValOrDefault)
  override def focusPosition: SeqAction[Double] = SeqActionF.embed(sys.focusPosition.safeValOrDefault)
  override def focalPlaneMask: SeqAction[String] = SeqActionF.embed(sys.mask.safeValOrDefault)
  override def beamSplitter: SeqAction[String] = SeqActionF.embed(sys.beamSplitter.safeValOrDefault)
  override def windowCover: SeqAction[String] = SeqActionF.embed(sys.windowCover.safeValOrDefault)
  override def framesPerCycle: SeqAction[Int] = SeqActionF.embed(sys.framesPerCycle.safeValOrDefault)
  override def headerTiming: SeqAction[String] = SeqActionF.embed(sys.hdrTiming.safeValOrDefault.map{
    case 1 => "BEFORE"
    case 2 => "AFTER"
    case 3 => "BOTH"
    case _ => "INDEF"
  })
  override def lnrs: SeqAction[Int] = SeqActionF.embed(sys.lnrs.safeValOrDefault)
  override def mode: SeqAction[String] = SeqActionF.embed(sys.mode.safeValOrDefault.map{
    case 0 => "STARE"
    case 1 => "SEP"
    case 2 => "CHOP"
    case 3 => "CHOP2"
    case 4 => "TEST"
    case _ => "INDEF"
  })
  override def numberDigitalAverage: SeqAction[Int] = SeqActionF.embed(sys.digitalAverageCount.safeValOrDefault)
  override def pupilViewer: SeqAction[String] = SeqActionF.embed(sys.pupilViewer.safeValOrDefault)
  override def detectorTemperature: SeqAction[Double] = SeqActionF.embed(sys.detectorTemp.safeValOrDefault)
  override def mountTemperature: SeqAction[Double] = SeqActionF.embed(sys.mountTemp.safeValOrDefault)
  override def µcodeName: SeqAction[String] = SeqActionF.embed(sys.µcodeName.safeValOrDefault)
  override def µcodeType: SeqAction[String] = SeqActionF.embed(sys.µcodeType.safeValOrDefault.map{
    case 1 => "RRD"
    case 2 => "RDD"
    case 3 => "RD"
    case 4 => "SRB"
    case _ => "INDEF"
  })
  override def cl1VoltageDD: SeqAction[Double] = SeqActionF.embed(sys.vddCl1.safeValOrDefault)
  override def cl2VoltageDD: SeqAction[Double] = SeqActionF.embed(sys.vddCl2.safeValOrDefault)
  override def ucVoltage: SeqAction[Double] = SeqActionF.embed(sys.vddUc.safeValOrDefault)
  override def detectorVoltage: SeqAction[Double] = SeqActionF.embed(sys.detectorVDetBias.safeValOrDefault)
  override def cl1VoltageGG: SeqAction[Double] = SeqActionF.embed(sys.vggCl1.safeValOrDefault)
  override def cl2VoltageGG: SeqAction[Double] = SeqActionF.embed(sys.vggCl2.safeValOrDefault)
  override def setVoltage: SeqAction[Double] = SeqActionF.embed(sys.detectorVSetBias.safeValOrDefault)
  override def observationEpoch: SeqAction[Double] = SeqActionF.embed(sys.obsEpoch.safeValOrDefault)
}
