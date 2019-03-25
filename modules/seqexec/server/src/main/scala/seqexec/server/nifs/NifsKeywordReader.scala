// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.effect.IO
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

object NifsKeywordReaderDummy extends NifsKeywordReader[IO] {
  override def aperture: IO[String]          = IO.pure(StrDefault)
  override def biasPwr: IO[Double]           = IO.pure(DoubleDefault)
  override def centralWavelength: IO[Double] = IO.pure(DoubleDefault)
  override def coadds: IO[Int]               = IO.pure(IntDefault)
  override def dcName: IO[String]            = IO.pure(StrDefault)
  override def exposureTime: IO[Double]      = IO.pure(DoubleDefault)
  override def exposureMode: IO[String]      = IO.pure(StrDefault)
  override def filter: IO[String]            = IO.pure(StrDefault)
  override def grating: IO[String]           = IO.pure(StrDefault)
  override def imagingMirror: IO[String]     = IO.pure(StrDefault)
  override def maskOffset: IO[Double]        = IO.pure(DoubleDefault)
  override def numberOfFowSamples: IO[Int]   = IO.pure(IntDefault)
  override def numberOfPeriods: IO[Int]      = IO.pure(IntDefault)
  override def period: IO[Double]            = IO.pure(DoubleDefault)
  override def readTime: IO[Double]          = IO.pure(DoubleDefault)
  override def windowCover: IO[String]       = IO.pure(StrDefault)
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

object NifsKeywordReaderImpl
    extends NifsKeywordReader[IO]
    with NifsKeywordReaderLUT {
  val sys = NifsEpics.instance
  override def aperture: IO[String] =
    sys.mask
      .map(_ === Invalid)
      .ifM(sys.lastSelectedMask, sys.mask)
      .map(_.flatMap(MaskKeywordLUT.get))
      .safeValOrDefault

  override def biasPwr: IO[Double] = sys.biasPwr.safeValOrDefault

  override def centralWavelength: IO[Double] =
    sys.centralWavelength.safeValOrDefault

  override def coadds: IO[Int]          = sys.coadds.safeValOrDefault

  override def dcName: IO[String]       = sys.dcName.safeValOrDefault

  override def exposureTime: IO[Double] = sys.exposureTime.safeValOrDefault

  override def exposureMode: IO[String] = sys.exposureMode.safeValOrDefault

  override def filter: IO[String] =
    sys.filter.map(_.flatMap(FilterKeywordLUT.get)).safeValOrDefault

  override def grating: IO[String] =
    sys.disperser
      .map(_ === Invalid)
      .ifM(sys.lastSelectedDisperser, sys.disperser)
      .map(_.flatMap(DisperserKeywordLUT.get))
      .safeValOrDefault

  override def imagingMirror: IO[String] = sys.imagingMirror.safeValOrDefault

  override def maskOffset: IO[Double]    = sys.maskOffset.safeValOrDefault

  override def numberOfFowSamples: IO[Int] =
    sys.numberOfFowSamples.safeValOrDefault

  override def numberOfPeriods: IO[Int] = sys.numberOfPeriods.safeValOrDefault

  override def period: IO[Double]       = sys.period.safeValOrDefault

  override def readTime: IO[Double]     = sys.readTime.safeValOrDefault

  override def windowCover: IO[String]  = sys.windowCover.safeValOrDefault
}
