// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.effect.IO
import seqexec.server.keywords._

trait NifsKeywordReader[F[_]] {
  def biasPwr: F[Double]
  def centralWavelength: F[Double]
  def coadds: F[Int]
  def dcName: F[String]
  def exposureTime: F[Double]
  def exposureMode: F[String]
  def imagingMirror: F[String]
  def maskOffset: F[Double]
  def numberOfFowSamples: F[Int]
  def numberOfPeriods: F[Int]
  def period: F[Double]
  def readTime: F[Double]
  def windowCover: F[String]
}

object NifsKeywordReaderDummy extends NifsKeywordReader[IO] {
  override def biasPwr: IO[Double] = IO.pure(DoubleDefault)
  override def centralWavelength: IO[Double] = IO.pure(DoubleDefault)
  override def coadds: IO[Int] = IO.pure(IntDefault)
  override def dcName: IO[String] = IO.pure(StrDefault)
  override def exposureTime: IO[Double] = IO.pure(DoubleDefault)
  override def exposureMode: IO[String] = IO.pure(StrDefault)
  override def imagingMirror: IO[String] = IO.pure(StrDefault)
  override def maskOffset: IO[Double] = IO.pure(DoubleDefault)
  override def numberOfFowSamples: IO[Int] = IO.pure(IntDefault)
  override def numberOfPeriods: IO[Int] = IO.pure(IntDefault)
  override def period: IO[Double] = IO.pure(DoubleDefault)
  override def readTime: IO[Double] = IO.pure(DoubleDefault)
  override def windowCover: IO[String] = IO.pure(StrDefault)
}

object NifsKeywordReaderImpl extends NifsKeywordReader[IO] {
  val sys = NifsEpics.instance
  override def biasPwr: IO[Double] = sys.biasPwr.safeDefault
  override def centralWavelength: IO[Double] = sys.centralWavelength.safeDefault
  override def coadds: IO[Int] = sys.coadds.safeDefault
  override def dcName: IO[String] = sys.dcName.safeDefault
  override def exposureTime: IO[Double] = sys.exposureTime.safeDefault
  override def exposureMode: IO[String] = sys.exposureMode.safeDefault
  override def imagingMirror: IO[String] = sys.imagingMirror.safeDefault
  override def maskOffset: IO[Double] = sys.maskOffset.safeDefault
  override def numberOfFowSamples: IO[Int] = sys.numberOfFowSamples.safeDefault
  override def numberOfPeriods: IO[Int] = sys.numberOfPeriods.safeDefault
  override def period: IO[Double] = sys.period.safeDefault
  override def readTime: IO[Double] = sys.readTime.safeDefault
  override def windowCover: IO[String] = sys.windowCover.safeDefault
}
