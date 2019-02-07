// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import cats.effect.IO
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
  override def arrayId: SeqAction[String] = sys.arrayId.toSeqActionDefault
  override def arrayType: SeqAction[String] = sys.arrayType.toSeqActionDefault
  override def camera: SeqAction[String] = sys.camera.toSeqActionDefault
  override def coadds: SeqAction[Int] = sys.coadds.toSeqActionDefault
  override def exposureTime: SeqAction[Double] = sys.integrationTime.toSeqActionDefault
  override def filter1: SeqAction[String] = sys.filter1.toSeqActionDefault
  override def filter2: SeqAction[String] = sys.filter2.toSeqActionDefault
  override def filter3: SeqAction[String] = sys.filter3.toSeqActionDefault
  override def focusName: SeqAction[String] = sys.focus.toSeqActionDefault
  override def focusPosition: SeqAction[Double] = sys.focusPosition.toSeqActionDefault
  override def focalPlaneMask: SeqAction[String] = sys.mask.toSeqActionDefault
  override def beamSplitter: SeqAction[String] = sys.beamSplitter.toSeqActionDefault
  override def windowCover: SeqAction[String] = sys.windowCover.toSeqActionDefault
  override def framesPerCycle: SeqAction[Int] = sys.framesPerCycle.toSeqActionDefault
  override def headerTiming: SeqAction[String] = sys.hdrTiming.toSeqActionDefault.map{
    case 1 => "BEFORE"
    case 2 => "AFTER"
    case 3 => "BOTH"
    case _ => "INDEF"
  }
  override def lnrs: SeqAction[Int] = sys.lnrs.toSeqActionDefault
  override def mode: SeqAction[String] = sys.mode.toSeqActionDefault.map{
    case 0 => "STARE"
    case 1 => "SEP"
    case 2 => "CHOP"
    case 3 => "CHOP2"
    case 4 => "TEST"
    case _ => "INDEF"
  }
  override def numberDigitalAverage: SeqAction[Int] = sys.digitalAverageCount.toSeqActionDefault
  override def pupilViewer: SeqAction[String] = sys.pupilViewer.toSeqActionDefault
  override def detectorTemperature: SeqAction[Double] = sys.detectorTemp.toSeqActionDefault
  override def mountTemperature: SeqAction[Double] = sys.mountTemp.toSeqActionDefault
  override def µcodeName: SeqAction[String] = sys.µcodeName.toSeqActionDefault
  override def µcodeType: SeqAction[String] = sys.µcodeType.toSeqActionDefault.map{
    case 1 => "RRD"
    case 2 => "RDD"
    case 3 => "RD"
    case 4 => "SRB"
    case _ => "INDEF"
  }
  override def cl1VoltageDD: SeqAction[Double] = sys.vddCl1.toSeqActionDefault
  override def cl2VoltageDD: SeqAction[Double] = sys.vddCl2.toSeqActionDefault
  override def ucVoltage: SeqAction[Double] = sys.vddUc.toSeqActionDefault
  override def detectorVoltage: SeqAction[Double] = sys.detectorVDetBias.toSeqActionDefault
  override def cl1VoltageGG: SeqAction[Double] = sys.vggCl1.toSeqActionDefault
  override def cl2VoltageGG: SeqAction[Double] = sys.vggCl2.toSeqActionDefault
  override def setVoltage: SeqAction[Double] = sys.detectorVSetBias.toSeqActionDefault
  override def observationEpoch: SeqAction[Double] = sys.obsEpoch.toSeqActionDefault
}
