// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Applicative
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
  def cwfs4Counts: F[Double]
}

object GemsKeywordReaderDummy {
  def apply[F[_]: Applicative]: GemsKeywordReader[F] = new GemsKeywordReader[F] {
    override def sadc: F[String] = strDefault
    override def dichroic: F[Double] = doubleDefault
    override def astrometricMode: F[String] = strDefault
    override def nadc: F[String] = strDefault
    override def lgswfs1Counts: F[Double] = doubleDefault
    override def lgswfs2Counts: F[Double] = doubleDefault
    override def lgswfs3Counts: F[Double] = doubleDefault
    override def lgswfs4Counts: F[Double] = doubleDefault
    override def lgswfs5Counts: F[Double] = doubleDefault
    override def lgsLoop: F[String] = strDefault
    override def ttLoop: F[String] = strDefault
    override def focLoop: F[String] = strDefault
    override def flexLoop: F[String] = strDefault
    override def lgsStrhl: F[Double] = doubleDefault
    override def rZeroVal: F[Double] = doubleDefault
    override def cnSquare1: F[Double] = doubleDefault
    override def cnSquare2: F[Double] = doubleDefault
    override def cnSquare3: F[Double] = doubleDefault
    override def cnSquare4: F[Double] = doubleDefault
    override def cnSquare5: F[Double] = doubleDefault
    override def cnSquare6: F[Double] = doubleDefault
    override def odgw1X: F[Int] = intDefault
    override def odgw1Y: F[Int] = intDefault
    override def odgwSize: F[Int] = intDefault
    override def odgw1Counts: F[Double] = doubleDefault
    override def odgw2X: F[Int] = intDefault
    override def odgw2Y: F[Int] = intDefault
    override def odgw2Counts: F[Double] = doubleDefault
    override def odgw3X: F[Int] = intDefault
    override def odgw3Y: F[Int] = intDefault
    override def odgw3Counts: F[Double] = doubleDefault
    override def odgw4X: F[Int] = intDefault
    override def odgw4Y: F[Int] = intDefault
    override def odgw4Counts: F[Double] = doubleDefault
    override def cwfs1Counts: F[Double] = doubleDefault
    override def cwfs2Counts: F[Double] = doubleDefault
    override def cwfs3Counts: F[Double] = doubleDefault
    override def cwfs4Counts: F[Double] = doubleDefault
  }
}
