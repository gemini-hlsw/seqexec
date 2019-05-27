// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.Applicative
import cats.effect.Sync
import cats.effect.LiftIO
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
  val Invalid: Option[String] = "INVALID".some

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
  def apply[F[_]: Sync: LiftIO]: NifsKeywordReader[F] = new NifsKeywordReader[F] {
    val sys = NifsEpics.instance
    override def aperture: F[String] =
      sys.mask
        .map(_ === Invalid)
        .ifM(sys.lastSelectedMask, sys.mask)
        .map(_.flatMap(MaskKeywordLUT.get))
        .safeValOrDefault.to[F]

    override def biasPwr: F[Double] = sys.biasPwr.safeValOrDefault.to[F]

    override def centralWavelength: F[Double] =
      sys.centralWavelength.safeValOrDefault.to[F]

    override def coadds: F[Int]          = sys.coadds.safeValOrDefault.to[F]

    override def dcName: F[String]       = sys.dcName.safeValOrDefault.to[F]

    override def exposureTime: F[Double] = sys.exposureTime.safeValOrDefault.to[F]

    override def exposureMode: F[String] = sys.exposureMode.safeValOrDefault.to[F]

    override def filter: F[String] =
      sys.filter.map(_.flatMap(FilterKeywordLUT.get)).safeValOrDefault.to[F]

    override def grating: F[String] =
      sys.disperser
        .map(_ === Invalid)
        .ifM(sys.lastSelectedDisperser, sys.disperser)
        .map(_.flatMap(DisperserKeywordLUT.get))
        .safeValOrDefault.to[F]

    override def imagingMirror: F[String] = sys.imagingMirror.safeValOrDefault.to[F]

    override def maskOffset: F[Double]    = sys.maskOffset.safeValOrDefault.to[F]

    override def numberOfFowSamples: F[Int] =
      sys.numberOfFowSamples.safeValOrDefault.to[F]

    override def numberOfPeriods: F[Int] = sys.numberOfPeriods.safeValOrDefault.to[F]

    override def period: F[Double]       = sys.period.safeValOrDefault.to[F]

    override def readTime: F[Double]     = sys.readTime.safeValOrDefault.to[F]

    override def windowCover: F[String]  = sys.windowCover.safeValOrDefault.to[F]
  }
}
