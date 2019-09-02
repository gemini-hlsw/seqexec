// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import seqexec.server.keywords._

trait NifsKeywordReader[F[_]] {
  def aperture          : F[String]
  def biasPwr           : F[Double]
  def centralWavelength : F[Double]
  def coadds            : F[Int]
  def dcName            : F[String]
  def exposureTime      : F[Double]
  def exposureMode      : F[String]
  def filter            : F[String]
  def grating           : F[String]
  def imagingMirror     : F[String]
  def maskOffset        : F[Double]
  def numberOfFowSamples: F[Int]
  def numberOfPeriods   : F[Int]
  def period            : F[Double]
  def readTime          : F[Double]
  def windowCover       : F[String]
}

object NifsKeywordReaderDummy {
  def apply[F[_]: Applicative]: NifsKeywordReader[F] = new NifsKeywordReader[F] {
    override def aperture: F[String]          = strDefault[F]
    override def biasPwr: F[Double]           = doubleDefault[F]
    override def centralWavelength: F[Double] = doubleDefault[F]
    override def coadds: F[Int]               = intDefault[F]
    override def dcName: F[String]            = strDefault[F]
    override def exposureTime: F[Double]      = doubleDefault[F]
    override def exposureMode: F[String]      = strDefault[F]
    override def filter: F[String]            = strDefault[F]
    override def grating: F[String]           = strDefault[F]
    override def imagingMirror: F[String]     = strDefault[F]
    override def maskOffset: F[Double]        = doubleDefault[F]
    override def numberOfFowSamples: F[Int]   = intDefault[F]
    override def numberOfPeriods: F[Int]      = intDefault[F]
    override def period: F[Double]            = doubleDefault[F]
    override def readTime: F[Double]          = doubleDefault[F]
    override def windowCover: F[String]       = strDefault[F]
  }
}

trait NifsKeywordReaderLUT {
  val Invalid: String = "INVALID"

  val DisperserKeywordLUT: Map[String, String] =
    Map(
      "Z"       -> "Z_G5602",
      "H"       -> "H_G5604",
      "J"       -> "J_G5603",
      "K"       -> "K_G5605",
      "Mirror"  -> "Mirror_G5601",
      "K_Short" -> "K_Short_G5606",
      "K_Long"  -> "K_Long_G5607"
    )

  val MaskKeywordLUT: Map[String, String] =
    Map(
      "3.0_Mask"       -> "3.0_Mask_G5610",
      "0.1_Hole"       -> "0.1_Hole_G5611",
      "0.2_Hole"       -> "0.2_Hole_G5612",
      "0.2_Hole_Array" -> "0.2_Hole_Array_G5613",
      "0.2_Slit"       -> "0.2_Slit_G5614",
      "Ronchi_Screen"  -> "Ronchi_Screen_G5615",
      "0.1_Occ_Disc"   -> "0.1_Occ_Disc_G5616",
      "0.2_Occ_Disc"   -> "0.2_Occ_Disc_G5617",
      "0.5_Occ_Disc"   -> "0.5_Occ_Disc_G5618",
      "KG3_ND_Filter"  -> "KG3_ND_Filter_G5619",
      "KG5_ND_Filter"  -> "KG5_ND_Filter_G5620",
      "Blocked"        -> "Blocked_G5621"
    )

  val FilterKeywordLUT: Map[String, String] =
    Map(
      "ZJ"           -> "ZJ_G0601",
      "JH"           -> "JH_G0602",
      "HK"           -> "HK_G0603",
      "HK+Polarizer" -> "HK+Polarizer_G0604",
      "Blocked"      -> "Blocked_G0605"
    )
}

object NifsKeywordReaderEpics extends NifsKeywordReaderLUT {
  def apply[F[_]: Sync](sys: NifsEpics[F]): NifsKeywordReader[F] = new NifsKeywordReader[F] {
    override def aperture: F[String] =
      sys.mask
        .map(_ === Invalid)
        .ifM(sys.lastSelectedMask, sys.mask)
        .map(MaskKeywordLUT.get)
        .safeValOrDefault

    override def biasPwr: F[Double] = sys.biasPwr.safeValOrDefault

    override def centralWavelength: F[Double] =
      sys.centralWavelength.safeValOrDefault

    override def coadds: F[Int]          = sys.coadds.safeValOrDefault

    override def dcName: F[String]       = sys.dcName.safeValOrDefault

    override def exposureTime: F[Double] = sys.exposureTime.safeValOrDefault

    override def exposureMode: F[String] = sys.exposureMode.safeValOrDefault

    override def filter: F[String] =
      sys.filter.map(FilterKeywordLUT.get).safeValOrDefault

    override def grating: F[String] =
      sys.disperser
        .map(_ === Invalid)
        .ifM(sys.lastSelectedDisperser, sys.disperser)
        .map(DisperserKeywordLUT.get)
        .safeValOrDefault

    override def imagingMirror: F[String] = sys.imagingMirror.safeValOrDefault

    override def maskOffset: F[Double]    = sys.maskOffset.safeValOrDefault

    override def numberOfFowSamples: F[Int] =
      sys.numberOfFowSamples.safeValOrDefault

    override def numberOfPeriods: F[Int] = sys.numberOfPeriods.safeValOrDefault

    override def period: F[Double]       = sys.period.safeValOrDefault

    override def readTime: F[Double]     = sys.readTime.safeValOrDefault

    override def windowCover: F[String]  = sys.windowCover.safeValOrDefault
  }
}
