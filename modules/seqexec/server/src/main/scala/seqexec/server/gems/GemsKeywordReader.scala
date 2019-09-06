// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import seqexec.server.gsaoi.GsaoiEpics
import seqexec.server.keywords._

trait GemsKeywordReader[F[_]] {
  def sadc: F[String]
  def dichroic: F[Double]
  def astrometricMode: F[String]
  def nadc: F[String]
  def lgswfs1Counts: F[Double]
  def lgswfs2Counts: F[Double]
  def lgswfs3Counts: F[Double]
  def lgswfs4Counts: F[Double]
  def lgswfs5Counts: F[Double]
  def lgsLoop: F[String]
  def ttLoop: F[String]
  def focLoop: F[String]
  def flexLoop: F[String]
  def lgsStrhl: F[Double]
  def rZeroVal: F[Double]
  def cnSquare1: F[Double]
  def cnSquare2: F[Double]
  def cnSquare3: F[Double]
  def cnSquare4: F[Double]
  def cnSquare5: F[Double]
  def cnSquare6: F[Double]
  def odgwSize: F[Int]
  def odgw1X: F[Int]
  def odgw1Y: F[Int]
  def odgw1Counts: F[Double]
  def odgw2X: F[Int]
  def odgw2Y: F[Int]
  def odgw2Counts: F[Double]
  def odgw3X: F[Int]
  def odgw3Y: F[Int]
  def odgw3Counts: F[Double]
  def odgw4X: F[Int]
  def odgw4Y: F[Int]
  def odgw4Counts: F[Double]
  def cwfs1Counts: F[Double]
  def cwfs2Counts: F[Double]
  def cwfs3Counts: F[Double]
}

object GemsKeywordReaderDummy {
  def apply[F[_]: Applicative]: GemsKeywordReader[F] = new GemsKeywordReader[F] {
    override def sadc: F[String] = strDefault[F]
    override def dichroic: F[Double] = doubleDefault[F]
    override def astrometricMode: F[String] = strDefault[F]
    override def nadc: F[String] = strDefault[F]
    override def lgswfs1Counts: F[Double] = doubleDefault[F]
    override def lgswfs2Counts: F[Double] = doubleDefault[F]
    override def lgswfs3Counts: F[Double] = doubleDefault[F]
    override def lgswfs4Counts: F[Double] = doubleDefault[F]
    override def lgswfs5Counts: F[Double] = doubleDefault[F]
    override def lgsLoop: F[String] = strDefault[F]
    override def ttLoop: F[String] = strDefault[F]
    override def focLoop: F[String] = strDefault[F]
    override def flexLoop: F[String] = strDefault[F]
    override def lgsStrhl: F[Double] = doubleDefault[F]
    override def rZeroVal: F[Double] = doubleDefault[F]
    override def cnSquare1: F[Double] = doubleDefault[F]
    override def cnSquare2: F[Double] = doubleDefault[F]
    override def cnSquare3: F[Double] = doubleDefault[F]
    override def cnSquare4: F[Double] = doubleDefault[F]
    override def cnSquare5: F[Double] = doubleDefault[F]
    override def cnSquare6: F[Double] = doubleDefault[F]
    override def odgw1X: F[Int] = intDefault[F]
    override def odgw1Y: F[Int] = intDefault[F]
    override def odgwSize: F[Int] = intDefault[F]
    override def odgw1Counts: F[Double] = doubleDefault[F]
    override def odgw2X: F[Int] = intDefault[F]
    override def odgw2Y: F[Int] = intDefault[F]
    override def odgw2Counts: F[Double] = doubleDefault[F]
    override def odgw3X: F[Int] = intDefault[F]
    override def odgw3Y: F[Int] = intDefault[F]
    override def odgw3Counts: F[Double] = doubleDefault[F]
    override def odgw4X: F[Int] = intDefault[F]
    override def odgw4Y: F[Int] = intDefault[F]
    override def odgw4Counts: F[Double] = doubleDefault[F]
    override def cwfs1Counts: F[Double] = doubleDefault[F]
    override def cwfs2Counts: F[Double] = doubleDefault[F]
    override def cwfs3Counts: F[Double] = doubleDefault[F]
  }
}

object GemsKeywordReaderEpics {
  def apply[F[_]: Sync](epics: => GemsEpics[F], gsaoiEpics: => GsaoiEpics[F]): GemsKeywordReader[F] = new GemsKeywordReader[F] {

    override def sadc: F[String] = epics.scienceAdcLoopActive.map(if(_) "ON" else "OFF").safeValOrDefault

    override def dichroic: F[Double] = epics.beamSplitterState.map{
      case "1" => 0.85
      case "2" => 1.0
      case _   => DoubleDefault
    }.safeValOrDefault

    override def astrometricMode: F[String] = epics.astroMode.map{
      case "None"    => "off"
      case "Regular" => "regular"
      case "Good"    => "good"
      case "Best"    => "best"
      case a         => a
    }.safeValOrDefault

    override def nadc: F[String] = epics.ngsAdcLoopActive.map(if(_) "ON" else "OFF").safeValOrDefault

    private def lgswfsFlux(idx: Long): F[Double] = epics.lgsFlux.map(_.get(idx).map(_.toDouble))
      .safeValOrDefault

    override def lgswfs1Counts: F[Double] = lgswfsFlux(1)

    override def lgswfs2Counts: F[Double] = lgswfsFlux(2)

    override def lgswfs3Counts: F[Double] = lgswfsFlux(3)

    override def lgswfs4Counts: F[Double] = lgswfsFlux(4)

    override def lgswfs5Counts: F[Double] = lgswfsFlux(5)

    override def lgsLoop: F[String] = epics.lgsLoop.map(_.name).safeValOrDefault

    override def ttLoop: F[String] = epics.ttLoop.map(_.name).safeValOrDefault

    override def focLoop: F[String] = epics.focusLoop.map(_.name).safeValOrDefault

    override def flexLoop: F[String] = epics.flexureLoop.map(_.name).safeValOrDefault

    override def lgsStrhl: F[Double] = epics.lgsStrehl.safeValOrDefault

    override def rZeroVal: F[Double] = epics.rZero.safeValOrDefault

    private def cnSum(idxs: List[Long]): F[Double] = epics.cnSquare.map{vs => idxs.map(vs.get).combineAll}
      .safeValOrDefault

    override def cnSquare1: F[Double] = cnSum(List(16))

    override def cnSquare2: F[Double] = cnSum(List(17, 18))

    override def cnSquare3: F[Double] = cnSum(List(19, 20))

    override def cnSquare4: F[Double] = cnSum(List(21, 22))

    override def cnSquare5: F[Double] = cnSum(List(23, 24))

    override def cnSquare6: F[Double] = cnSum(List.range(25L, 31L))

    override def odgwSize: F[Int] = gsaoiEpics.odgwSize.safeValOrDefault

    override def odgw1X: F[Int] = gsaoiEpics.odgw1X.safeValOrDefault

    override def odgw1Y: F[Int] = gsaoiEpics.odgw1Y.safeValOrDefault

    override def odgw1Counts: F[Double] = gsaoiEpics.odgw1Counts.safeValOrDefault

    override def odgw2X: F[Int] = gsaoiEpics.odgw2X.safeValOrDefault

    override def odgw2Y: F[Int] = gsaoiEpics.odgw2Y.safeValOrDefault

    override def odgw2Counts: F[Double] = gsaoiEpics.odgw2Counts.safeValOrDefault

    override def odgw3X: F[Int] = gsaoiEpics.odgw3X.safeValOrDefault

    override def odgw3Y: F[Int] = gsaoiEpics.odgw3Y.safeValOrDefault

    override def odgw3Counts: F[Double] = gsaoiEpics.odgw3Counts.safeValOrDefault

    override def odgw4X: F[Int] = gsaoiEpics.odgw4X.safeValOrDefault

    override def odgw4Y: F[Int] = gsaoiEpics.odgw4Y.safeValOrDefault

    override def odgw4Counts: F[Double] = gsaoiEpics.odgw4Counts.safeValOrDefault

    private def cwfsFlux(idx: Long): F[Double] = epics.ngsFlux.map(_.get(idx).map(_.toDouble)).safeValOrDefault

    override def cwfs1Counts: F[Double] = cwfsFlux(1)

    override def cwfs2Counts: F[Double] = cwfsFlux(2)

    override def cwfs3Counts: F[Double] = cwfsFlux(3)

  }
}