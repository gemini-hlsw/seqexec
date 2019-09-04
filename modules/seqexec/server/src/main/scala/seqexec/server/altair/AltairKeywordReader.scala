// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.effect.Sync
import cats.Applicative
import cats.implicits._
import seqexec.server.keywords._

trait AltairKeywordReader[F[_]] {
  def aofreq: F[Double]
  def aocounts: F[Double]
  def aoseeing: F[Double]
  def aowfsx: F[Double]
  def aowfsy: F[Double]
  def aowfsz: F[Double]
  def aogain: F[Double]
  def aoncpa: F[String]
  def ngndfilt: F[String]
  def astar: F[String]
  def aoflex: F[String]
  def lgustage: F[String]
  def aobs: F[String]

  // LGS
  def lgdfocus: F[Double]
  def lgttcnts: F[Double]
  def lgttexp: F[Int]
  def lgsfcnts: F[Double]
  def lgsfexp: F[Double]
  def fsmtip: F[Double]
  def fsmtilt: F[Double]
  def lgzmpos: F[Double]
  def naalt: F[Double]
  def nathick: F[Double]
  def lgndfilt: F[String]
  def lgttiris: F[String]
}

object AltairKeywordReaderDummy {
  def apply[F[_]: Applicative]: AltairKeywordReader[F] = new AltairKeywordReader[F] {
    override def aofreq: F[Double]   = doubleDefault[F]
    override def aocounts: F[Double] = doubleDefault[F]
    override def aoseeing: F[Double] = doubleDefault[F]
    override def aowfsx: F[Double]   = doubleDefault[F]
    override def aowfsy: F[Double]   = doubleDefault[F]
    override def aowfsz: F[Double]   = doubleDefault[F]
    override def aogain: F[Double]   = doubleDefault[F]
    override def aoncpa: F[String]   = strDefault[F]
    override def ngndfilt: F[String] = strDefault[F]
    override def astar: F[String]    = strDefault[F]
    override def aoflex: F[String]   = strDefault[F]
    override def lgustage: F[String] = strDefault[F]
    override def aobs: F[String]     = strDefault[F]
    override def lgdfocus: F[Double] = doubleDefault[F]
    override def lgttcnts: F[Double] = doubleDefault[F]
    override def lgttexp: F[Int]     = intDefault[F]
    override def lgsfcnts: F[Double] = doubleDefault[F]
    override def lgsfexp: F[Double]  = doubleDefault[F]
    override def fsmtip: F[Double]   = doubleDefault[F]
    override def fsmtilt: F[Double]  = doubleDefault[F]
    override def lgzmpos: F[Double]  = doubleDefault[F]
    override def naalt: F[Double]    = doubleDefault[F]
    override def nathick: F[Double]  = doubleDefault[F]
    override def lgndfilt: F[String] = strDefault[F]
    override def lgttiris: F[String] = strDefault[F]
  }
}

trait AltairKeywordReaderLUT {
  val AOFlensKeywordLUT: Map[String, String] =
    Map(
      "FLENS" -> "IN",
      "0"     -> "INDEF"
    )

}

object AltairKeywordReaderEpics extends AltairKeywordReaderLUT {
  def apply[F[_]: Sync](sys: AltairEpics[F]): AltairKeywordReader[F] = new AltairKeywordReader[F] {

    override def aofreq: F[Double] =
      sys.aoexpt
        .map(1.0 / _.toDouble)
        .safeValOrDefault
    override def aocounts: F[Double] = sys.aocounts.safeValOrDefault
    override def aoseeing: F[Double] =
      sys.aoseeing.map(_.toDouble).safeValOrDefault
    override def aowfsx: F[Double]   = sys.aowfsx.safeValOrDefault
    override def aowfsy: F[Double]   = sys.aowfsy.safeValOrDefault
    override def aowfsz: F[Double]   = sys.aowfsz.safeValOrDefault
    override def aogain: F[Double]   = sys.aogain.safeValOrDefault
    override def aoncpa: F[String]   = sys.aoncpa.safeValOrDefault
    override def ngndfilt: F[String] = sys.ngndfilt.safeValOrDefault
    override def astar: F[String] =
      sys.astar.safeValOrDefault.map(AOFlensKeywordLUT.getOrElse(_, "OUT"))
    override def aoflex: F[String]   = sys.aoflex.safeValOrDefault
    override def lgustage: F[String] = sys.lgustage.safeValOrDefault
    override def aobs: F[String]     = sys.aobs.safeValOrDefault

    // LGS
    override def lgdfocus: F[Double] = sys.lgdfocus.safeValOrDefault
    override def lgttcnts: F[Double] =
      (for {
        apd1 <- sys.apd1
        apd2 <- sys.apd2
        apd3 <- sys.apd3
        apd4 <- sys.apd4
      } yield
        (apd1 + apd2 + apd3 + apd4).toDouble
    ).safeValOrDefault
    override def lgttexp: F[Int]     = sys.lgttexp.safeValOrDefault
    override def lgsfcnts: F[Double] = sys.lgsfcnts.safeValOrDefault
    override def lgsfexp: F[Double]  = sys.lgsfexp.safeValOrDefault
    override def fsmtip: F[Double]   = sys.fsmtip.safeValOrDefault
    override def fsmtilt: F[Double]  = sys.fsmtilt.safeValOrDefault
    override def lgzmpos: F[Double]  = sys.lgzmpos.safeValOrDefault
    override def naalt: F[Double] = {
      val modela: Double = 210.0
      val modelb: Double = 1.01
      val f: Double      = 128.0
      val r = for {
        roofO <- sys.lgzmpos
        zaO   <- sys.aoza
      } yield {
        val d = (modela - roofO) / modelb
        val k = f + (d / 1000.0)
        val r = f * k / (k - f)
        r * math.cos(zaO.toRadians) / 1000.0
      }
      r.safeValOrDefault
    }
    override def nathick: F[Double]  = sys.nathick.safeValOrDefault
    override def lgndfilt: F[String] = sys.lgndfilt.safeValOrDefault
    override def lgttiris: F[String] = sys.lgttiris.safeValOrDefault
  }
}
