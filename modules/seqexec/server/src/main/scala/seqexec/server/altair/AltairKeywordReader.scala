// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.effect.LiftIO
import cats.effect.Sync
import cats.Applicative
import cats.data.Nested
import cats.implicits._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.altair.AltairParams.CassRotator
import edu.gemini.spModel.gemini.altair.InstAltair.CASS_ROTATOR_PROP
import seqexec.server.keywords._
import seqexec.server.ConfigUtilOps._

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
  def aocrfollow: F[String]
}

class AltairKeywordReaderDummy[F[_]: Applicative]
    extends AltairKeywordReader[F] {
  override def aofreq: F[Double]     = doubleDefault[F]
  override def aocounts: F[Double]   = doubleDefault[F]
  override def aoseeing: F[Double]   = doubleDefault[F]
  override def aowfsx: F[Double]     = doubleDefault[F]
  override def aowfsy: F[Double]     = doubleDefault[F]
  override def aowfsz: F[Double]     = doubleDefault[F]
  override def aogain: F[Double]     = doubleDefault[F]
  override def aoncpa: F[String]     = strDefault[F]
  override def ngndfilt: F[String]   = strDefault[F]
  override def astar: F[String]      = strDefault[F]
  override def aoflex: F[String]     = strDefault[F]
  override def lgustage: F[String]   = strDefault[F]
  override def aobs: F[String]       = strDefault[F]
  override def aocrfollow: F[String] = strDefault[F]
}

trait AltairKeywordReaderLUT {
  val AOFlensKeywordLUT: Map[String, String] =
    Map(
      "FLENS" -> "IN",
      "0"     -> "INDEF"
    )

  def crFollowValue(cr: AOCRFollow): String = cr match {
    case AOCRFollow.Following => "yes"
    case AOCRFollow.Fixed     => "no"
  }
}

class AltairKeywordReaderImpl[F[_]: Sync: LiftIO](config: Config)
    extends AltairKeywordReader[F]
    with AltairKeywordReaderLUT {
  val sys = AltairEpics.instance

  override def aofreq: F[Double] =
    Nested(sys.aoexpt)
      .filter(_ =!= 0.0f)
      .map(1 / _.toDouble)
      .value
      .safeValOrDefault
      .to[F]
  override def aocounts: F[Double] = sys.aocounts.safeValOrDefault.to[F]
  override def aoseeing: F[Double] =
    Nested(sys.aoseeing).map(_.toDouble).value.safeValOrDefault.to[F]
  override def aowfsx: F[Double]   = sys.aowfsx.safeValOrDefault.to[F]
  override def aowfsy: F[Double]   = sys.aowfsy.safeValOrDefault.to[F]
  override def aowfsz: F[Double]   = sys.aowfsz.safeValOrDefault.to[F]
  override def aogain: F[Double]   = sys.aogain.safeValOrDefault.to[F]
  override def aoncpa: F[String]   = sys.aoncpa.safeValOrDefault.to[F]
  override def ngndfilt: F[String] = sys.ngndfilt.safeValOrDefault.to[F]
  override def astar: F[String] =
    sys.astar.safeValOrDefault.map(AOFlensKeywordLUT.getOrElse(_, "OUT")).to[F]
  override def aoflex: F[String]   = sys.aoflex.safeValOrDefault.to[F]
  override def lgustage: F[String] = sys.lgustage.safeValOrDefault.to[F]
  override def aobs: F[String]     = sys.aobs.safeValOrDefault.to[F]
  override def aocrfollow: F[String] = Sync[F].delay {
    config
      .extractAOAs[CassRotator](CASS_ROTATOR_PROP)
      .map {
        case CassRotator.FIXED     => AOCRFollow.Fixed
        case CassRotator.FOLLOWING => AOCRFollow.Following
      }
      .map(crFollowValue)
      .getOrElse(Indef)
  }
}
