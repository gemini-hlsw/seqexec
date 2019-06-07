// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.config.{DynamicConfig, StaticConfig}
import gem.config.GmosConfig._
import gem.enum._
import gsp.math.{Offset, Wavelength}
import gsp.math.arb.{ ArbOffset, ArbTime, ArbWavelength }
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen, Gen}

import java.time.Duration


trait ArbGmos {

  import ArbOffset._
  import ArbEnumerated._
  import ArbTime._
  import ArbWavelength._


  // Static Config

  implicit val arbGmosShuffleOffset: Arbitrary[GmosShuffleOffset] =
    Arbitrary(Gen.posNum[Int].map(GmosShuffleOffset.unsafeFromRowCount))

  implicit val cogGmosShuffleOffset: Cogen[GmosShuffleOffset] =
    Cogen[Int].contramap(_.detectorRows)

  implicit val arbGmosShuffleCycles: Arbitrary[GmosShuffleCycles] =
    Arbitrary(Gen.posNum[Int].map(GmosShuffleCycles.unsafeFromCycleCount))

  implicit val cogGmosShuffleCycles: Cogen[GmosShuffleCycles] =
    Cogen[Int].contramap(_.toInt)

  implicit val arbGmosNodAndShuffle: Arbitrary[GmosNodAndShuffle] =
    Arbitrary {
      for {
        a <- arbitrary[Offset]
        b <- arbitrary[Offset]
        e <- arbitrary[GmosEOffsetting]
        o <- arbitrary[GmosShuffleOffset]
        c <- arbitrary[GmosShuffleCycles]
      } yield GmosNodAndShuffle(a, b, e, o, c)
    }

  implicit val cogGmosNodAndShuffle: Cogen[GmosNodAndShuffle] =
    Cogen[(Offset, Offset, GmosEOffsetting, GmosShuffleOffset, GmosShuffleCycles)]
      .contramap(n => (n.posA, n.posB, n.eOffset, n.shuffle, n.cycles))

  implicit val arbGmosCustomRoiEntry: Arbitrary[GmosCustomRoiEntry] =
    Arbitrary {
      for {
        xMin <- Gen.posNum[Short]
        yMin <- Gen.posNum[Short]
        xRng <- Gen.posNum[Short]
        yRng <- Gen.posNum[Short]
      } yield GmosCustomRoiEntry.unsafeFromDescription(xMin, yMin, xRng, yRng)
    }

  implicit val cogGmosCustomRoiEntry: Cogen[GmosCustomRoiEntry] =
    Cogen[(Short, Short, Short, Short)].contramap(c => (c.xMin, c.yMin, c.xRange, c.yRange))

  implicit val arbGmosCommonStaticConfig: Arbitrary[GmosCommonStaticConfig] =
    Arbitrary {
      for {
        d <- arbitrary[GmosDetector]
        p <- arbitrary[MosPreImaging]
        n <- arbitrary[Option[GmosNodAndShuffle]]
        c <- Gen.choose(1, 5)
        r <- Gen.listOfN(c, arbitrary[GmosCustomRoiEntry])
      } yield GmosCommonStaticConfig(d, p, n, r.toSet)
    }

  implicit val cogGmosCommonStaticConfig: Cogen[GmosCommonStaticConfig] =
    Cogen[(GmosDetector, MosPreImaging, Option[GmosNodAndShuffle], List[GmosCustomRoiEntry])]
      .contramap(c => (c.detector, c.mosPreImaging, c.nodAndShuffle, c.customRois.toList))

  implicit val arbGmosNorthStatic: Arbitrary[StaticConfig.GmosN] =
    Arbitrary {
      for {
        c <- arbitrary[GmosCommonStaticConfig]
        s <- arbitrary[GmosNorthStageMode]
      } yield StaticConfig.GmosN(c, s)
    }

  implicit val cogGmosNorthStatic: Cogen[StaticConfig.GmosN] =
    Cogen[(GmosCommonStaticConfig, GmosNorthStageMode)]
      .contramap(g => (g.common, g.stageMode))

  implicit val arbGmosSouthStatic: Arbitrary[StaticConfig.GmosS] =
    Arbitrary {
      for {
        c <- arbitrary[GmosCommonStaticConfig]
        s <- arbitrary[GmosSouthStageMode]
      } yield StaticConfig.GmosS(c, s)
    }

  implicit val cogGmosSouthStatic: Cogen[StaticConfig.GmosS] =
    Cogen[(GmosCommonStaticConfig, GmosSouthStageMode)]
      .contramap(g => (g.common, g.stageMode))


  // Dynamic Config

  implicit val arbGmosCcdReadout: Arbitrary[GmosCcdReadout] =
    Arbitrary {
      for {
        x <- arbitrary[GmosXBinning]
        y <- arbitrary[GmosYBinning]
        c <- arbitrary[GmosAmpCount]
        g <- arbitrary[GmosAmpGain]
        r <- arbitrary[GmosAmpReadMode]
      } yield GmosCcdReadout(x, y, c, g, r)
    }

  implicit val cogGmosCcdReadout: Cogen[GmosCcdReadout] =
    Cogen[(GmosXBinning, GmosYBinning, GmosAmpCount, GmosAmpGain, GmosAmpReadMode)]
      .contramap(c => (c.xBinning, c.yBinning, c.ampCount, c.ampGain, c.ampReadMode))

  implicit val arbGmosCommonDynamic: Arbitrary[GmosCommonDynamicConfig] =
    Arbitrary {
      for {
        c <- arbitrary[GmosCcdReadout]
        d <- arbitrary[GmosDtax]
        e <- arbitrary[Duration]
        r <- arbitrary[GmosRoi]
      } yield GmosCommonDynamicConfig(c, d, e, r)
    }

  implicit val cogGmosCommonDynamic: Cogen[GmosCommonDynamicConfig] =
    Cogen[(GmosCcdReadout, GmosDtax, Duration, GmosRoi)]
      .contramap(c => (c.ccdReadout, c.dtaxOffset, c.exposureTime, c.roi))

  implicit val arbGmosCustomMask: Arbitrary[GmosCustomMask] =
    Arbitrary {
      for {
        m <- Gen.alphaStr.map(_.take(32))
        w <- arbitrary[GmosCustomSlitWidth]
      } yield GmosCustomMask(m, w)
    }

  implicit val cogGmosCustomMask: Cogen[GmosCustomMask] =
    Cogen[(String, GmosCustomSlitWidth)].contramap(g => (g.maskDefinitionFilename, g.slitWidth))

  implicit val arbGmosNorthGrating: Arbitrary[GmosGrating[GmosNorthDisperser]] =
    Arbitrary {
      for {
        d <- arbitrary[GmosNorthDisperser]
        o <- arbitrary[GmosDisperserOrder]
        w <- Gen.choose(3000, 12000).map(Wavelength.fromAngstroms.unsafeGet)
      } yield GmosGrating(d, o, w)
    }

  implicit val cogGmosNorthGrating: Cogen[GmosGrating[GmosNorthDisperser]] =
    Cogen[(GmosNorthDisperser, GmosDisperserOrder, Wavelength)]
      .contramap(g => (g.disperser, g.order, g.wavelength))

  implicit val arbGmosSouthGrating: Arbitrary[GmosGrating[GmosSouthDisperser]] =
    Arbitrary {
      for {
        d <- arbitrary[GmosSouthDisperser]
        o <- arbitrary[GmosDisperserOrder]
        w <- Gen.choose(3000, 12000).map(Wavelength.fromAngstroms.unsafeGet)
      } yield GmosGrating(d, o, w)
    }

  implicit val cogGmosSouthGrating: Cogen[GmosGrating[GmosSouthDisperser]] =
    Cogen[(GmosSouthDisperser, GmosDisperserOrder, Wavelength)]
      .contramap(g => (g.disperser, g.order, g.wavelength))

  implicit val arbGmosNorthDynamic: Arbitrary[DynamicConfig.GmosN] =
    Arbitrary {
      for {
        c <- arbitrary[GmosCommonDynamicConfig]
        g <- arbitrary[Option[GmosGrating[GmosNorthDisperser]]]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[Option[Either[GmosCustomMask, GmosNorthFpu]]]
      } yield DynamicConfig.GmosN(c, g, f, u)
    }

  implicit val cogGmosNorthDynamic: Cogen[DynamicConfig.GmosN] =
    Cogen[(GmosCommonDynamicConfig, Option[GmosGrating[GmosNorthDisperser]], Option[GmosNorthFilter], Option[Either[GmosCustomMask, GmosNorthFpu]])]
      .contramap(g => (g.common, g.grating, g.filter, g.fpu))

  implicit val arbGmosSouthDynamic: Arbitrary[DynamicConfig.GmosS] =
    Arbitrary {
      for {
        c <- arbitrary[GmosCommonDynamicConfig]
        g <- arbitrary[Option[GmosGrating[GmosSouthDisperser]]]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[Option[Either[GmosCustomMask, GmosSouthFpu]]]
      } yield DynamicConfig.GmosS(c, g, f, u)
    }

  implicit val cogGmosSouthDynamic: Cogen[DynamicConfig.GmosS] =
    Cogen[(GmosCommonDynamicConfig, Option[GmosGrating[GmosSouthDisperser]], Option[GmosSouthFilter], Option[Either[GmosCustomMask, GmosSouthFpu]])]
      .contramap(g => (g.common, g.grating, g.filter, g.fpu))

}

object ArbGmos extends ArbGmos
