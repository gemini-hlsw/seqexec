// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.Applicative
import cats.effect.Sync
import cats.effect.LiftIO
import seqexec.server.keywords._

trait GnirsKeywordReader[F[_]] {
  def arrayId: F[String]
  def arrayType: F[String]
  def detectorBias: F[Double]
  def filter1: F[String]
  def filterWheel1Pos: F[Int]
  def filter2: F[String]
  def filterWheel2Pos: F[Int]
  def camera: F[String]
  def cameraPos: F[Int]
  def decker: F[String]
  def deckerPos: F[Int]
  def slit: F[String]
  def slitPos: F[Int]
  def prism: F[String]
  def prismPos: F[Int]
  def grating: F[String]
  def gratingPos: F[Int]
  def gratingWavelength: F[Double]
  def gratingOrder: F[Int]
  def gratingTilt: F[Double]
  def focus: F[String]
  def focusPos: F[Int]
  def acquisitionMirror: F[String]
  def windowCover: F[String]
  def obsEpoch: F[Double]
}

object GnirsKeywordReaderDummy {
  def apply[F[_]: Applicative]: GnirsKeywordReader[F] = new GnirsKeywordReader[F] {
    override def arrayId: F[String] = strDefault[F]
    override def arrayType: F[String] = strDefault[F]
    override def detectorBias: F[Double] = doubleDefault[F]
    override def filter1: F[String] = strDefault[F]
    override def filterWheel1Pos: F[Int] = intDefault[F]
    override def filter2: F[String] = strDefault[F]
    override def filterWheel2Pos: F[Int] = intDefault[F]
    override def camera: F[String] = strDefault[F]
    override def cameraPos: F[Int] = intDefault[F]
    override def decker: F[String] = strDefault[F]
    override def deckerPos: F[Int] = intDefault[F]
    override def slit: F[String] = strDefault[F]
    override def slitPos: F[Int] = intDefault[F]
    override def prism: F[String] = strDefault[F]
    override def prismPos: F[Int] = intDefault[F]
    override def grating: F[String] = strDefault[F]
    override def gratingPos: F[Int] = intDefault[F]
    override def gratingWavelength: F[Double] = doubleDefault[F]
    override def gratingOrder: F[Int] = intDefault[F]
    override def gratingTilt: F[Double] = doubleDefault[F]
    override def focus: F[String] = strDefault[F]
    override def focusPos: F[Int] = intDefault[F]
    override def acquisitionMirror: F[String] = strDefault[F]
    override def windowCover: F[String] = strDefault[F]
    override def obsEpoch: F[Double] = doubleDefault[F]
  }
}

object GnirsKeywordReaderEpics {
  def apply[F[_]: Sync: LiftIO]: GnirsKeywordReader[F] = new GnirsKeywordReader[F] {
    private val F = implicitly[Sync[F]]
    private val sys = GnirsEpics.instance

    // TODO make GnirsEpics referentially transparent
    override def arrayId: F[String] = F.delay(sys.arrayId).safeValOrDefault
    override def arrayType: F[String] = F.delay(sys.arrayType).safeValOrDefault
    override def detectorBias: F[Double] = F.delay(sys.detBias).safeValOrDefault
    override def filter1: F[String] = F.delay(sys.filter1).safeValOrDefault
    override def filterWheel1Pos: F[Int] = F.delay(sys.filter1Eng).safeValOrDefault
    override def filter2: F[String] = F.delay(sys.filter2).safeValOrDefault
    override def filterWheel2Pos: F[Int] = F.delay(sys.filter2Eng).safeValOrDefault
    override def camera: F[String] = F.delay(sys.camera).safeValOrDefault
    override def cameraPos: F[Int] = F.delay(sys.cameraEng).safeValOrDefault
    override def decker: F[String] = F.delay(sys.decker).safeValOrDefault
    override def deckerPos: F[Int] = F.delay(sys.deckerEng).safeValOrDefault
    override def slit: F[String] = F.delay(sys.slitWidth).safeValOrDefault
    override def slitPos: F[Int] = F.delay(sys.slitEng).safeValOrDefault
    override def prism: F[String] = F.delay(sys.prism).safeValOrDefault
    override def prismPos: F[Int] = F.delay(sys.prismEng).safeValOrDefault
    override def grating: F[String] = F.delay(sys.grating).safeValOrDefault
    override def gratingPos: F[Int] = F.delay(sys.gratingEng).safeValOrDefault
    override def gratingWavelength: F[Double] = F.delay(sys.centralWavelength).safeValOrDefault
    override def gratingOrder: F[Int] = F.delay(sys.gratingOrder).safeValOrDefault
    override def gratingTilt: F[Double] = F.delay(sys.gratingTilt).safeValOrDefault
    override def focus: F[String] = F.delay(sys.focus).safeValOrDefault
    override def focusPos: F[Int] = F.delay(sys.focusEng).safeValOrDefault
    override def acquisitionMirror: F[String] = F.delay(sys.acqMirror).safeValOrDefault
    override def windowCover: F[String] = F.delay(sys.cover).safeValOrDefault
    override def obsEpoch: F[Double] = F.delay(sys.obsEpoch).safeValOrDefault
  }
}
