// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gnirs

import edu.gemini.seqexec.server.SeqAction
import edu.gemini.seqexec.server.Header
import edu.gemini.seqexec.server.Header.Implicits._

trait GnirsKeywordReader {
  def getArrayId: SeqAction[String]
  def getArrayType: SeqAction[String]
  def getDetectorBias: SeqAction[Double]
  def getFilter1: SeqAction[String]
  def getFilterWheel1Pos: SeqAction[Int]
  def getFilter2: SeqAction[String]
  def getFilterWheel2Pos: SeqAction[Int]
  def getCamera: SeqAction[String]
  def getCameraPos: SeqAction[Int]
  def getDecker: SeqAction[String]
  def getDeckerPos: SeqAction[Int]
  def getSlit: SeqAction[String]
  def getSlitPos: SeqAction[Int]
  def getPrism: SeqAction[String]
  def getPrismPos: SeqAction[Int]
  def getGrating: SeqAction[String]
  def getGratingPos: SeqAction[Int]
  def getGratingWavelength: SeqAction[Double]
  def getGratingOrder: SeqAction[Int]
  def getGratingTilt: SeqAction[Double]
  def getFocus: SeqAction[String]
  def getFocusPos: SeqAction[Int]
  def getAcquisitionMirror: SeqAction[String]
  def getWindowCover: SeqAction[String]
  def getObsEpoch: SeqAction[Double]
}

object GnirsKeywordReaderDummy extends GnirsKeywordReader {
  override def getArrayId: SeqAction[String] = SeqAction(Header.StrDefault)
  override def getArrayType: SeqAction[String] = SeqAction(Header.StrDefault)
  override def getDetectorBias: SeqAction[Double] = SeqAction(Header.DoubleDefault)
  override def getFilter1: SeqAction[String] = SeqAction(Header.StrDefault)
  override def getFilterWheel1Pos: SeqAction[Int] = SeqAction(Header.IntDefault)
  override def getFilter2: SeqAction[String] = SeqAction(Header.StrDefault)
  override def getFilterWheel2Pos: SeqAction[Int] = SeqAction(Header.IntDefault)
  override def getCamera: SeqAction[String] = SeqAction(Header.StrDefault)
  override def getCameraPos: SeqAction[Int] = SeqAction(Header.IntDefault)
  override def getDecker: SeqAction[String] = SeqAction(Header.StrDefault)
  override def getDeckerPos: SeqAction[Int] = SeqAction(Header.IntDefault)
  override def getSlit: SeqAction[String] = SeqAction(Header.StrDefault)
  override def getSlitPos: SeqAction[Int] = SeqAction(Header.IntDefault)
  override def getPrism: SeqAction[String] = SeqAction(Header.StrDefault)
  override def getPrismPos: SeqAction[Int] = SeqAction(Header.IntDefault)
  override def getGrating: SeqAction[String] = SeqAction(Header.StrDefault)
  override def getGratingPos: SeqAction[Int] = SeqAction(Header.IntDefault)
  override def getGratingWavelength: SeqAction[Double] = SeqAction(Header.DoubleDefault)
  override def getGratingOrder: SeqAction[Int] = SeqAction(Header.IntDefault)
  override def getGratingTilt: SeqAction[Double] = SeqAction(Header.DoubleDefault)
  override def getFocus: SeqAction[String] = SeqAction(Header.StrDefault)
  override def getFocusPos: SeqAction[Int] = SeqAction(Header.IntDefault)
  override def getAcquisitionMirror: SeqAction[String] = SeqAction(Header.StrDefault)
  override def getWindowCover: SeqAction[String] = SeqAction(Header.StrDefault)
  override def getObsEpoch: SeqAction[Double] = SeqAction(Header.DoubleDefault)
}

object GnirsKeywordReaderImpl extends GnirsKeywordReader {
  override def getArrayId: SeqAction[String] = GnirsEpics.instance.arrayId.toSeqAction
  override def getArrayType: SeqAction[String] = GnirsEpics.instance.arrayType.toSeqAction
  override def getDetectorBias: SeqAction[Double] = GnirsEpics.instance.detBias.toSeqAction
  override def getFilter1: SeqAction[String] = GnirsEpics.instance.filter1.toSeqAction
  override def getFilterWheel1Pos: SeqAction[Int] = GnirsEpics.instance.filter1Eng.toSeqAction
  override def getFilter2: SeqAction[String] = GnirsEpics.instance.filter2.toSeqAction
  override def getFilterWheel2Pos: SeqAction[Int] = GnirsEpics.instance.filter2Eng.toSeqAction
  override def getCamera: SeqAction[String] = GnirsEpics.instance.camera.toSeqAction
  override def getCameraPos: SeqAction[Int] = GnirsEpics.instance.cameraEng.toSeqAction
  override def getDecker: SeqAction[String] = GnirsEpics.instance.decker.toSeqAction
  override def getDeckerPos: SeqAction[Int] = GnirsEpics.instance.deckerEng.toSeqAction
  override def getSlit: SeqAction[String] = GnirsEpics.instance.slitWidth.toSeqAction
  override def getSlitPos: SeqAction[Int] = GnirsEpics.instance.slitEng.toSeqAction
  override def getPrism: SeqAction[String] = GnirsEpics.instance.prism.toSeqAction
  override def getPrismPos: SeqAction[Int] = GnirsEpics.instance.prismEng.toSeqAction
  override def getGrating: SeqAction[String] = GnirsEpics.instance.grating.toSeqAction
  override def getGratingPos: SeqAction[Int] = GnirsEpics.instance.gratingEng.toSeqAction
  override def getGratingWavelength: SeqAction[Double] = GnirsEpics.instance.centralWavelength.toSeqAction
  override def getGratingOrder: SeqAction[Int] = GnirsEpics.instance.gratingOrder.toSeqAction
  override def getGratingTilt: SeqAction[Double] = GnirsEpics.instance.gratingTilt.toSeqAction
  override def getFocus: SeqAction[String] = GnirsEpics.instance.focus.toSeqAction
  override def getFocusPos: SeqAction[Int] = GnirsEpics.instance.focusEng.toSeqAction
  override def getAcquisitionMirror: SeqAction[String] = GnirsEpics.instance.acqMirror.toSeqAction
  override def getWindowCover: SeqAction[String] = GnirsEpics.instance.cover.toSeqAction
  override def getObsEpoch: SeqAction[Double] = GnirsEpics.instance.obsEpoch.toSeqAction
}
