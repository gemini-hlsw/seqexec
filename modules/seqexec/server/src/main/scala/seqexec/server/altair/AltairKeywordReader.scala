// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.effect.LiftIO
import cats.Applicative
import cats.data.Nested
import cats.implicits._
import seqexec.server.keywords._

trait AltairKeywordReader[F[_]] {
  def aoexpt           : F[Double]
  def aocounts           : F[Double]
  def aoseeing           : F[Double]
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
}

class AltairKeywordReaderDummy[F[_]: Applicative] extends AltairKeywordReader[F] {
  override def aoexpt: F[Double]           = doubleDefault[F]
  override def aocounts: F[Double]           = doubleDefault[F]
  override def aoseeing: F[Double]           = doubleDefault[F]
  override def aowfsx: F[Double] = doubleDefault[F]
  override def aowfsy: F[Double] = doubleDefault[F]
  override def aowfsz: F[Double] = doubleDefault[F]
  override def aogain: F[Double] = doubleDefault[F]
  override def aoncpa: F[String] = strDefault[F]
  override def ngndfilt: F[String] = strDefault[F]
  override def astar: F[String] = strDefault[F]
  override def aoflex: F[String] = strDefault[F]
  override def lgustage: F[String] = strDefault[F]
  override def aobs: F[String] = strDefault[F]
}

trait AltairKeywordReaderLUT {
//   val Invalid: Option[String] = "INVALID".some
//
//   val DisperserKeywordLUT: Map[String, String] =
//     Map(
//       "Z"       -> "Z_G5602",
//       "H"       -> "H_G5604",
//       "J"       -> "J_G5603",
//       "K"       -> "K_G5605",
//       "Mirror"  -> "Mirror_G5601",
//       "K_Short" -> "K_Short_G5606",
//       "K_Long"  -> "K_Long_G5607"
//     )
//
//   val MaskKeywordLUT: Map[String, String] =
//     Map(
//       "3.0_Mask"       -> "3.0_Mask_G5610",
//       "0.1_Hole"       -> "0.1_Hole_G5611",
//       "0.2_Hole"       -> "0.2_Hole_G5612",
//       "0.2_Hole_Array" -> "0.2_Hole_Array_G5613",
//       "0.2_Slit"       -> "0.2_Slit_G5614",
//       "Ronchi_Screen"  -> "Ronchi_Screen_G5615",
//       "0.1_Occ_Disc"   -> "0.1_Occ_Disc_G5616",
//       "0.2_Occ_Disc"   -> "0.2_Occ_Disc_G5617",
//       "0.5_Occ_Disc"   -> "0.5_Occ_Disc_G5618",
//       "KG3_ND_Filter"  -> "KG3_ND_Filter_G5619",
//       "KG5_ND_Filter"  -> "KG5_ND_Filter_G5620",
//       "Blocked"        -> "Blocked_G5621"
//     )
//
//   val FilterKeywordLUT: Map[String, String] =
//     Map(
//       "ZJ"           -> "ZJ_G0601",
//       "JH"           -> "JH_G0602",
//       "HK"           -> "HK_G0603",
//       "HK+Polarizer" -> "HK+Polarizer_G0604",
//       "Blocked"      -> "Blocked_G0605"
//     )
}

class AltairKeywordReaderImpl[F[_]: LiftIO]
    extends AltairKeywordReader[F]
    with AltairKeywordReaderLUT {
  val sys = AltairEpics.instance
  override def aoexpt: F[Double] = Nested(sys.aoexpt).map(1 / _).value.safeValOrDefault.to[F]
  override def aocounts: F[Double] = sys.aocounts.safeValOrDefault.to[F]
  override def aoseeing: F[Double] = sys.aoseeing.safeValOrDefault.to[F]
  override def aowfsx: F[Double] = sys.aowfsx.safeValOrDefault.to[F]
  override def aowfsy: F[Double] = sys.aowfsy.safeValOrDefault.to[F]
  override def aowfsz: F[Double] = sys.aowfsz.safeValOrDefault.to[F]
  override def aogain: F[Double] = sys.aogain.safeValOrDefault.to[F]
  override def aoncpa: F[String] = sys.aoncpa.safeValOrDefault.to[F]
  override def ngndfilt: F[String] = sys.ngndfilt.safeValOrDefault.to[F]
  override def astar: F[String] = sys.astar.safeValOrDefault.to[F]
  override def aoflex: F[String] = sys.aoflex.safeValOrDefault.to[F]
  override def lgustage: F[String] = sys.lgustage.safeValOrDefault.to[F]
  override def aobs: F[String] = sys.aobs.safeValOrDefault.to[F]

}
