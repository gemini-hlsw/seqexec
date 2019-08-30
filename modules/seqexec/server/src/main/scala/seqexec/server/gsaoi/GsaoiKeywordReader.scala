// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import seqexec.server.keywords._

trait GsaoiKeywordReader[F[_]] {
  def obsElapsedTime: F[Double]
  def readInterval: F[Double]
  def upperFilter: F[String]
  def upperFilterEngPos: F[Int]
  def upperFilterHealth: F[String]
  def lowerFilter: F[String]
  def lowerFilterEngPos: F[Int]
  def lowerFilterHealth: F[String]
  def utilityWheel: F[String]
  def utilityWheelEngPos: F[Int]
  def utilityWheelHealth: F[String]
  def windowCover: F[String]
  def windowCoverEngPos: F[Int]
  def windowCoverHealth: F[String]
  def coldworkSurfaceTemperature: F[Double]
  def detectorTemperature: F[Double]
  def detectorHousingTemperature: F[Double]
  def dewarPressure: F[Double]
  def dateObs: F[String]
  def mjdobs: F[Double]
  def readMode: F[String]
  def expositionMode: F[String]
  def numberOfResets: F[Int]
  def resetDelay: F[Double]
  def readTime: F[Double]
  def bUnits: F[String]
  def dcName: F[String]
  def dcHealth: F[String]
  def simulationMode: F[String]
  def timingBoardCodeName: F[String]
  def dspCodeVersion: F[String]
  def pciBoardCodeName: F[String]
}

object GsaoiKeywordReaderDummy {
  def apply[F[_]: Applicative]: GsaoiKeywordReader[F] =
    new GsaoiKeywordReader[F] {
      override def obsElapsedTime: F[Double]             = doubleDefault[F]
      override def readInterval: F[Double]               = doubleDefault[F]
      override def upperFilter: F[String]                = strDefault[F]
      override def upperFilterEngPos: F[Int]             = intDefault[F]
      override def upperFilterHealth: F[String]          = strDefault[F]
      override def lowerFilter: F[String]                = strDefault[F]
      override def lowerFilterEngPos: F[Int]             = intDefault[F]
      override def lowerFilterHealth: F[String]          = strDefault[F]
      override def utilityWheel: F[String]               = strDefault[F]
      override def utilityWheelEngPos: F[Int]            = intDefault[F]
      override def utilityWheelHealth: F[String]         = strDefault[F]
      override def windowCover: F[String]                = strDefault[F]
      override def windowCoverEngPos: F[Int]             = intDefault[F]
      override def windowCoverHealth: F[String]          = strDefault[F]
      override def coldworkSurfaceTemperature: F[Double] = doubleDefault[F]
      override def detectorTemperature: F[Double]        = doubleDefault[F]
      override def detectorHousingTemperature: F[Double] = doubleDefault[F]
      override def dewarPressure: F[Double]              = doubleDefault[F]
      override def dateObs: F[String]                    = strDefault[F]
      override def mjdobs: F[Double]                     = doubleDefault[F]
      override def readMode: F[String]                   = strDefault[F]
      override def expositionMode: F[String]             = strDefault[F]
      override def numberOfResets: F[Int]                = intDefault[F]
      override def resetDelay: F[Double]                 = doubleDefault[F]
      override def readTime: F[Double]                   = doubleDefault[F]
      override def bUnits: F[String]                     = strDefault[F]
      override def dcName: F[String]                     = strDefault[F]
      override def dcHealth: F[String]                   = strDefault[F]
      override def simulationMode: F[String]             = strDefault[F]
      override def timingBoardCodeName: F[String]        = strDefault[F]
      override def dspCodeVersion: F[String]             = strDefault[F]
      override def pciBoardCodeName: F[String]           = strDefault[F]
    }
}

trait GsaoiLUT {
  def upperFilterLUT(f: String): String = f match {
    case "Z"            => "Z_G1101"
    case "J"            => "J_G1102"
    case "H"            => "H_G1103"
    case "K'"           => "Kprime_G1104"
    case "Ks"           => "Kshort_G1105"
    case "K"            => "K_G1106"
    case "J-continuum"  => "Jcont_G1107"
    case "H-continuum"  => "Hcont_G1108"
    case "CH4(short)"   => "CH4short_G1109"
    case "CH4(long)"    => "CH4long_G1110"
    case "Ks-continuum" => "Kcntshrt_G1111"
    case "Kl-continuum" => "Kcntlong_G1112"
    case "Diffuser1"    => "Diffuser1_G1113"
    case "Blocked"      => "Blocked1_G1114"
    case _              => f
  }

  def lowerFilterLUT(f: String): String = f match {
    case "HeI-1.083" => "HeI1083_G1115"
    case "HI-Pgamm"  => "PaG_G1116"
    case "HI-Pbeta"  => "PaB_G1117"
    case "FeII"      => "FeII_G1118"
    case "H2O"       => "H2O_G1119"
    case "HeI-2p2s"  => "HeI-2p2s_G1120"
    case "H2-1-0"    => "H2(1-0)_G1121"
    case "HI-Brgamm" => "BrG_G1122"
    case "H2-2-1"    => "H2(2-1)_G1123"
    case "CO"        => "CO2360_G1124"
    case "Diffuser2" => "Diffuser2_G1125"
    case "Hartman1"  => "Hartman1_G1126"
    case "Hartman2"  => "Hartman2_G1127"
    case "Blocked"   => "Blocked2_G1128"
    case _           => f
  }

  def utilityWheelLUT(f: String): String = f match {
    case "PupilViewer"    => "PupilView_G6101"
    case "FocusRetractor" => "ConvexLens_G6102"
    case "FocusExtender"  => "ConcaveLens_G6103"
    case _                => f
  }

  def windowCoverLUT(f: String): String = f match {
    case "Opened" => "OPEN"
    case "Closed" => "CLOSED"
    case _        => f
  }
}

object GsaoiKeywordReaderEpics extends GsaoiLUT {
  def apply[F[_]](sys: GsaoiEpics[F])(
    implicit F:        Sync[F]): GsaoiKeywordReader[F] =
    new GsaoiKeywordReader[F] {
      override def obsElapsedTime: F[Double] = sys.obsElapsedTime.safeValOrDefault
      override def readInterval: F[Double] = sys.readInterval.safeValOrDefault
      override def upperFilter: F[String] = sys.upperFilter.map(upperFilterLUT).safeValOrDefault
      override def upperFilterEngPos: F[Int] = sys.upperFilterEngPos.safeValOrDefault
      override def upperFilterHealth: F[String] = sys.upperFilterHealth.safeValOrDefault
      override def lowerFilter: F[String] = sys.lowerFilter.map(lowerFilterLUT).safeValOrDefault
      override def lowerFilterEngPos: F[Int] = sys.lowerFilterEngPos.safeValOrDefault
      override def lowerFilterHealth: F[String] = sys.lowerFilterHealth.safeValOrDefault
      override def utilityWheel: F[String] = sys.utilWheel.map(utilityWheelLUT).safeValOrDefault
      override def utilityWheelEngPos: F[Int] = sys.utilityWheelEngPos.safeValOrDefault
      override def utilityWheelHealth: F[String] = sys.utilityWheelHealth.safeValOrDefault
      override def windowCover: F[String] = sys.windowCover.map(windowCoverLUT).safeValOrDefault
      override def windowCoverEngPos: F[Int] = sys.windowCoverEngPos.safeValOrDefault
      override def windowCoverHealth: F[String] = sys.windowCoverHealth.safeValOrDefault
      override def coldworkSurfaceTemperature: F[Double] = sys.coldworkSurfaceTemperature.safeValOrDefault
      override def detectorTemperature: F[Double] = sys.detectorTemperature.safeValOrDefault
      override def detectorHousingTemperature: F[Double] = sys.detectorHousingTemperature.safeValOrDefault
      override def dewarPressure: F[Double] = sys.dewarPressure.map(p => Math.rint(p*100.0)/100.0).safeValOrDefault
      override def dateObs: F[String] = F.delay(LocalDate.now.format(DateTimeFormatter.ISO_LOCAL_DATE))
      override def mjdobs: F[Double] = sys.mjdobs.safeValOrDefault
      override def readMode: F[String] = sys.readMode.safeValOrDefault
      override def expositionMode: F[String] = sys.expositionMode.safeValOrDefault
      override def numberOfResets: F[Int] = sys.numberOfResets.safeValOrDefault
      override def resetDelay: F[Double] = sys.resetDelay.safeValOrDefault
      override def readTime: F[Double] = sys.readTime.safeValOrDefault
      override def bUnits: F[String] = sys.bUnits.safeValOrDefault
      override def dcName: F[String] = sys.dcName.safeValOrDefault
      override def dcHealth: F[String] = sys.dcHealth.safeValOrDefault
      override def simulationMode: F[String] = sys.simulationMode.safeValOrDefault
      override def timingBoardCodeName: F[String] = sys.timingBoardCodeName.safeValOrDefault
      override def dspCodeVersion: F[String] = sys.dspCodeVersion.safeValOrDefault
      override def pciBoardCodeName: F[String] = sys.pciBoardCodeName.safeValOrDefault
    }
}
