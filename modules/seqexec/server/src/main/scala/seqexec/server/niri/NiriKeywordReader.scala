// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import seqexec.server.SeqAction
import seqexec.server.keywords._

trait NiriKeywordReader {
  def arrayId: SeqAction[String]
  def arrayType: SeqAction[String]
  def camera: SeqAction[String]
  def coadds: SeqAction[Int]
  def exposureTime: SeqAction[Double]
  def filter1: SeqAction[String]
  def filter2: SeqAction[String]
  def filter3: SeqAction[String]
  def focusName: SeqAction[String]
  def focusPosition: SeqAction[Double]
  def focalPlaneMask: SeqAction[String]
  def beamSplitter: SeqAction[String]
  def windowCover: SeqAction[String]
  def framesPerCycle: SeqAction[Int]
  def headerTiming: SeqAction[String]
  def lnrs: SeqAction[Int]
  def mode: SeqAction[String]
  def numberDigitalAverage: SeqAction[Int]
  def pupilViewer: SeqAction[String]
  def detectorTemperature: SeqAction[Double]
  def mountTemperature: SeqAction[Double]
  def µcodeName: SeqAction[String]
  def µcodeType: SeqAction[String]
  def cl1VoltageDD: SeqAction[Double]
  def cl2VoltageDD: SeqAction[Double]
  def ucVoltage: SeqAction[Double]
  def detectorVoltage: SeqAction[Double]
  def cl1VoltageGG: SeqAction[Double]
  def cl2VoltageGG: SeqAction[Double]
  def setVoltage: SeqAction[Double]
  def observationEpoch: SeqAction[Double]
}

object NiriKeywordReaderDummy extends NiriKeywordReader {
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

object NiriKeywordReaderImpl extends NiriKeywordReader {
  val sys = NiriEpics.instance
  override def arrayId: SeqAction[String] = sys.arrayId.toSeqAction
  override def arrayType: SeqAction[String] = sys.arrayType.toSeqAction
  override def camera: SeqAction[String] = sys.camera.toSeqAction
  override def coadds: SeqAction[Int] = sys.coadds.toSeqAction
  override def exposureTime: SeqAction[Double] = sys.integrationTime.toSeqAction
  override def filter1: SeqAction[String] = sys.filter1.toSeqAction
  override def filter2: SeqAction[String] = sys.filter2.toSeqAction
  override def filter3: SeqAction[String] = sys.filter3.toSeqAction
  override def focusName: SeqAction[String] = sys.focus.toSeqAction
  override def focusPosition: SeqAction[Double] = sys.focusPosition.toSeqAction
  override def focalPlaneMask: SeqAction[String] = sys.mask.toSeqAction
  override def beamSplitter: SeqAction[String] = sys.beamSplitter.toSeqAction
  override def windowCover: SeqAction[String] = sys.windowCover.toSeqAction
  override def framesPerCycle: SeqAction[Int] = sys.framesPerCycle.toSeqAction
  override def headerTiming: SeqAction[String] = sys.hdrTiming.toSeqAction.map{
    case 1 => "BEFORE"
    case 2 => "AFTER"
    case 3 => "BOTH"
    case _ => "INDEF"
  }
  override def lnrs: SeqAction[Int] = sys.lnrs.toSeqAction
  override def mode: SeqAction[String] = sys.mode.toSeqAction.map{
    case 0 => "STARE"
    case 1 => "SEP"
    case 2 => "CHOP"
    case 3 => "CHOP2"
    case 4 => "TEST"
    case _ => "INDEF"
  }
  override def numberDigitalAverage: SeqAction[Int] = sys.digitalAverageCount.toSeqAction
  override def pupilViewer: SeqAction[String] = sys.pupilViewer.toSeqAction
  override def detectorTemperature: SeqAction[Double] = sys.detectorTemp.toSeqAction
  override def mountTemperature: SeqAction[Double] = sys.mountTemp.toSeqAction
  override def µcodeName: SeqAction[String] = sys.µcodeName.toSeqAction
  override def µcodeType: SeqAction[String] = sys.µcodeType.toSeqAction.map{
    case 1 => "RRD"
    case 2 => "RDD"
    case 3 => "RD"
    case 4 => "SRB"
    case _ => "INDEF"
  }
  override def cl1VoltageDD: SeqAction[Double] = sys.vddCl1.toSeqAction
  override def cl2VoltageDD: SeqAction[Double] = sys.vddCl2.toSeqAction
  override def ucVoltage: SeqAction[Double] = sys.vddUc.toSeqAction
  override def detectorVoltage: SeqAction[Double] = sys.detectorVDetBias.toSeqAction
  override def cl1VoltageGG: SeqAction[Double] = sys.vggCl1.toSeqAction
  override def cl2VoltageGG: SeqAction[Double] = sys.vggCl2.toSeqAction
  override def setVoltage: SeqAction[Double] = sys.detectorVSetBias.toSeqAction
  override def observationEpoch: SeqAction[Double] = sys.obsEpoch.toSeqAction
}