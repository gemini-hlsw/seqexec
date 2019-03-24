// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.effect.IO
import seqexec.server.SeqAction
import seqexec.server.SeqActionF
import seqexec.server.keywords._

trait GnirsKeywordReader[F[_]] {
  def getArrayId: SeqActionF[F, String]
  def getArrayType: SeqActionF[F, String]
  def getDetectorBias: SeqActionF[F, Double]
  def getFilter1: SeqActionF[F, String]
  def getFilterWheel1Pos: SeqActionF[F, Int]
  def getFilter2: SeqActionF[F, String]
  def getFilterWheel2Pos: SeqActionF[F, Int]
  def getCamera: SeqActionF[F, String]
  def getCameraPos: SeqActionF[F, Int]
  def getDecker: SeqActionF[F, String]
  def getDeckerPos: SeqActionF[F, Int]
  def getSlit: SeqActionF[F, String]
  def getSlitPos: SeqActionF[F, Int]
  def getPrism: SeqActionF[F, String]
  def getPrismPos: SeqActionF[F, Int]
  def getGrating: SeqActionF[F, String]
  def getGratingPos: SeqActionF[F, Int]
  def getGratingWavelength: SeqActionF[F, Double]
  def getGratingOrder: SeqActionF[F, Int]
  def getGratingTilt: SeqActionF[F, Double]
  def getFocus: SeqActionF[F, String]
  def getFocusPos: SeqActionF[F, Int]
  def getAcquisitionMirror: SeqActionF[F, String]
  def getWindowCover: SeqActionF[F, String]
  def getObsEpoch: SeqActionF[F, Double]
}

object GnirsKeywordReaderDummy extends GnirsKeywordReader[IO] {
  override def getArrayId: SeqActionF[IO, String] = SeqAction(StrDefault)
  override def getArrayType: SeqActionF[IO, String] = SeqAction(StrDefault)
  override def getDetectorBias: SeqActionF[IO, Double] = SeqAction(DoubleDefault)
  override def getFilter1: SeqActionF[IO, String] = SeqAction(StrDefault)
  override def getFilterWheel1Pos: SeqActionF[IO, Int] = SeqAction(IntDefault)
  override def getFilter2: SeqActionF[IO, String] = SeqAction(StrDefault)
  override def getFilterWheel2Pos: SeqActionF[IO, Int] = SeqAction(IntDefault)
  override def getCamera: SeqActionF[IO, String] = SeqAction(StrDefault)
  override def getCameraPos: SeqActionF[IO, Int] = SeqAction(IntDefault)
  override def getDecker: SeqActionF[IO, String] = SeqAction(StrDefault)
  override def getDeckerPos: SeqActionF[IO, Int] = SeqAction(IntDefault)
  override def getSlit: SeqActionF[IO, String] = SeqAction(StrDefault)
  override def getSlitPos: SeqActionF[IO, Int] = SeqAction(IntDefault)
  override def getPrism: SeqActionF[IO, String] = SeqAction(StrDefault)
  override def getPrismPos: SeqActionF[IO, Int] = SeqAction(IntDefault)
  override def getGrating: SeqActionF[IO, String] = SeqAction(StrDefault)
  override def getGratingPos: SeqActionF[IO, Int] = SeqAction(IntDefault)
  override def getGratingWavelength: SeqActionF[IO, Double] = SeqAction(DoubleDefault)
  override def getGratingOrder: SeqActionF[IO, Int] = SeqAction(IntDefault)
  override def getGratingTilt: SeqActionF[IO, Double] = SeqAction(DoubleDefault)
  override def getFocus: SeqActionF[IO, String] = SeqAction(StrDefault)
  override def getFocusPos: SeqActionF[IO, Int] = SeqAction(IntDefault)
  override def getAcquisitionMirror: SeqActionF[IO, String] = SeqAction(StrDefault)
  override def getWindowCover: SeqActionF[IO, String] = SeqAction(StrDefault)
  override def getObsEpoch: SeqActionF[IO, Double] = SeqAction(DoubleDefault)
}

object GnirsKeywordReaderImpl extends GnirsKeywordReader[IO] {
  override def getArrayId: SeqActionF[IO, String] = GnirsEpics.instance.arrayId.toSeqActionDefault
  override def getArrayType: SeqActionF[IO, String] = GnirsEpics.instance.arrayType.toSeqActionDefault
  override def getDetectorBias: SeqActionF[IO, Double] = GnirsEpics.instance.detBias.toSeqActionDefault
  override def getFilter1: SeqActionF[IO, String] = GnirsEpics.instance.filter1.toSeqActionDefault
  override def getFilterWheel1Pos: SeqActionF[IO, Int] = GnirsEpics.instance.filter1Eng.toSeqActionDefault
  override def getFilter2: SeqActionF[IO, String] = GnirsEpics.instance.filter2.toSeqActionDefault
  override def getFilterWheel2Pos: SeqActionF[IO, Int] = GnirsEpics.instance.filter2Eng.toSeqActionDefault
  override def getCamera: SeqActionF[IO, String] = GnirsEpics.instance.camera.toSeqActionDefault
  override def getCameraPos: SeqActionF[IO, Int] = GnirsEpics.instance.cameraEng.toSeqActionDefault
  override def getDecker: SeqActionF[IO, String] = GnirsEpics.instance.decker.toSeqActionDefault
  override def getDeckerPos: SeqActionF[IO, Int] = GnirsEpics.instance.deckerEng.toSeqActionDefault
  override def getSlit: SeqActionF[IO, String] = GnirsEpics.instance.slitWidth.toSeqActionDefault
  override def getSlitPos: SeqActionF[IO, Int] = GnirsEpics.instance.slitEng.toSeqActionDefault
  override def getPrism: SeqActionF[IO, String] = GnirsEpics.instance.prism.toSeqActionDefault
  override def getPrismPos: SeqActionF[IO, Int] = GnirsEpics.instance.prismEng.toSeqActionDefault
  override def getGrating: SeqActionF[IO, String] = GnirsEpics.instance.grating.toSeqActionDefault
  override def getGratingPos: SeqActionF[IO, Int] = GnirsEpics.instance.gratingEng.toSeqActionDefault
  override def getGratingWavelength: SeqActionF[IO, Double] = GnirsEpics.instance.centralWavelength.toSeqActionDefault
  override def getGratingOrder: SeqActionF[IO, Int] = GnirsEpics.instance.gratingOrder.toSeqActionDefault
  override def getGratingTilt: SeqActionF[IO, Double] = GnirsEpics.instance.gratingTilt.toSeqActionDefault
  override def getFocus: SeqActionF[IO, String] = GnirsEpics.instance.focus.toSeqActionDefault
  override def getFocusPos: SeqActionF[IO, Int] = GnirsEpics.instance.focusEng.toSeqActionDefault
  override def getAcquisitionMirror: SeqActionF[IO, String] = GnirsEpics.instance.acqMirror.toSeqActionDefault
  override def getWindowCover: SeqActionF[IO, String] = GnirsEpics.instance.cover.toSeqActionDefault
  override def getObsEpoch: SeqActionF[IO, Double] = GnirsEpics.instance.obsEpoch.toSeqActionDefault
}
