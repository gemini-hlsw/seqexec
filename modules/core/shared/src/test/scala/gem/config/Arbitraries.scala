// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import cats._

import gem.CoAdds
import gem.arb._
import gem.config.F2Config.F2FpuChoice
import gem.config.GmosConfig._
import gem.enum._
import gem.enum.Instrument._
import gem.math.{ Offset, Wavelength }

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

import java.time.Duration

trait Arbitraries {

  import ArbCoAdds._
  import ArbEnumerated._
  import ArbOffset._
  import ArbTime._
  import ArbWavelength._


  // Surely this is already defined somewhere?
  implicit val functorGen = new Functor[Gen] {
    def map[A, B](fa: Gen[A])(f: A => B): Gen[B] =
      fa.map(f)
  }

  implicit val applicativeGen = new Applicative[Gen] {
    def ap[A, B](gf: Gen[A => B])(ga: Gen[A]): Gen[B] =
      for {
        f <- gf
        a <- ga
      } yield f(a)

    def pure[A](a: A): Gen[A] =
      Gen.const(a)
  }

  implicit val arbMosPreImaging: Arbitrary[MosPreImaging] =
    Arbitrary(
      Gen.oneOf(MosPreImaging.IsMosPreImaging,
                MosPreImaging.IsNotMosPreImaging)
    )

  val genAcqCamStatic:   Gen[StaticConfig.AcqCam]    = Gen.const(StaticConfig.AcqCam()  )
  val genBhrosStatic:    Gen[StaticConfig.Bhros]     = Gen.const(StaticConfig.Bhros()   )
  val genGhostStatic:    Gen[StaticConfig.Ghost]     = Gen.const(StaticConfig.Ghost()   )
  val genGpiStatic:      Gen[StaticConfig.Gpi]       = Gen.const(StaticConfig.Gpi()     )
  val genGsaoiStatic:    Gen[StaticConfig.Gsaoi]     = Gen.const(StaticConfig.Gsaoi()   )
  val genMichelleStatic: Gen[StaticConfig.Michelle]  = Gen.const(StaticConfig.Michelle())
  val genNiciStatic:     Gen[StaticConfig.Nici]      = Gen.const(StaticConfig.Nici()    )
  val genNifsStatic:     Gen[StaticConfig.Nifs]      = Gen.const(StaticConfig.Nifs()    )
  val genNiriStatic:     Gen[StaticConfig.Niri]      = Gen.const(StaticConfig.Niri()    )
  val genPhoenixStatic:  Gen[StaticConfig.Phoenix]   = Gen.const(StaticConfig.Phoenix() )
  val genTrecsStatic:    Gen[StaticConfig.Trecs]     = Gen.const(StaticConfig.Trecs()   )
  val genVisitorStatic:  Gen[StaticConfig.Visitor]   = Gen.const(StaticConfig.Visitor() )

  val genF2Static: Gen[StaticConfig.Flamingos2] =
    arbitrary[MosPreImaging].map(StaticConfig.Flamingos2(_))

  implicit val arbGmosShuffleOffset =
    Arbitrary(Gen.posNum[Int].map(GmosConfig.GmosShuffleOffset.unsafeFromRowCount))

  implicit val cogGmosShuffleOffset: Cogen[GmosConfig.GmosShuffleOffset] =
    Cogen[Int].contramap(_.detectorRows)

  implicit val arbGmosShuffleCycles =
    Arbitrary(Gen.posNum[Int].map(GmosConfig.GmosShuffleCycles.unsafeFromCycleCount))

  implicit val cogGmosShuffleCycles: Cogen[GmosConfig.GmosShuffleCycles] =
    Cogen[Int].contramap(_.toInt)

  implicit val arbGmosNodAndShuffle =
    Arbitrary(
      for {
        a <- arbitrary[Offset]
        b <- arbitrary[Offset]
        e <- arbitrary[GmosEOffsetting]
        o <- arbitrary[GmosConfig.GmosShuffleOffset]
        c <- arbitrary[GmosConfig.GmosShuffleCycles]
      } yield GmosConfig.GmosNodAndShuffle(a, b, e, o, c)
    )

  implicit val cogGmosNodAndShuffle: Cogen[GmosConfig.GmosNodAndShuffle] =
    Cogen[(Offset, Offset, GmosEOffsetting, GmosShuffleOffset, GmosShuffleCycles)]
      .contramap(n => (n.posA, n.posB, n.eOffset, n.shuffle, n.cycles))

  implicit val arbGmosCustomRoiEntry =
    Arbitrary(
      for {
        xMin <- Gen.posNum[Short]
        yMin <- Gen.posNum[Short]
        xRng <- Gen.posNum[Short]
        yRng <- Gen.posNum[Short]
      } yield GmosConfig.GmosCustomRoiEntry.unsafeFromDescription(xMin, yMin, xRng, yRng)
    )

  implicit val cogGmosCustomRoiEntry: Cogen[GmosConfig.GmosCustomRoiEntry] =
    Cogen[(Short, Short, Short, Short)].contramap(c => (c.xMin, c.yMin, c.xRange, c.yRange))

  implicit val arbGmosCommonStaticConfig =
    Arbitrary(
      for {
        d <- arbitrary[GmosDetector]
        p <- arbitrary[MosPreImaging]
        n <- arbitrary[Option[GmosConfig.GmosNodAndShuffle]]
        c <- Gen.choose(1, 5)
        r <- Gen.listOfN(c, arbitrary[GmosConfig.GmosCustomRoiEntry])
      } yield GmosConfig.GmosCommonStaticConfig(d, p, n, r.toSet)
    )

  implicit val cogGmosCommonStaticConfig: Cogen[GmosConfig.GmosCommonStaticConfig] =
    Cogen[(GmosDetector, MosPreImaging, Option[GmosConfig.GmosNodAndShuffle], List[GmosConfig.GmosCustomRoiEntry])]
      .contramap(c => (c.detector, c.mosPreImaging, c.nodAndShuffle, c.customRois.toList))

  val genGmosNorthStatic: Gen[StaticConfig.GmosN] =
    for {
      c <- arbitrary[GmosConfig.GmosCommonStaticConfig]
      s <- arbitrary[GmosNorthStageMode]
    } yield StaticConfig.GmosN(c, s)

  val genGmosSouthStatic: Gen[StaticConfig.GmosS] =
    for {
      c <- arbitrary[GmosConfig.GmosCommonStaticConfig]
      s <- arbitrary[GmosSouthStageMode]
    } yield StaticConfig.GmosS(c, s)

  val genGnirsStatic: Gen[StaticConfig.Gnirs] =
    arbitrary[GnirsWellDepth].map(StaticConfig.Gnirs(_))

  def genStaticConfigOf(i: Instrument): Gen[StaticConfig] = {
    i match {
      case AcqCam     => genAcqCamStatic
      case Bhros      => genBhrosStatic
      case Flamingos2 => genF2Static
      case Ghost      => genGhostStatic
      case GmosN      => genGmosNorthStatic
      case GmosS      => genGmosSouthStatic
      case Gnirs      => genGnirsStatic
      case Gpi        => genGpiStatic
      case Gsaoi      => genGsaoiStatic
      case Michelle   => genMichelleStatic
      case Nici       => genNiciStatic
      case Nifs       => genNifsStatic
      case Niri       => genNiriStatic
      case Phoenix    => genPhoenixStatic
      case Trecs      => genTrecsStatic
      case Visitor    => genVisitorStatic
    }
  }

  implicit val arbGmosNStaticConfig: Arbitrary[StaticConfig.GmosN] =
    Arbitrary(genGmosNorthStatic)

  implicit val cogGmosNStaticConfig: Cogen[StaticConfig.GmosN] =
    Cogen[(GmosCommonStaticConfig, GmosNorthStageMode)]
      .contramap(g => (g.common, g.stageMode))

  implicit val arbGmosSStaticConfig: Arbitrary[StaticConfig.GmosS] =
    Arbitrary(genGmosSouthStatic)

  implicit val cogGmosSStaticConfig: Cogen[StaticConfig.GmosS] =
    Cogen[(GmosCommonStaticConfig, GmosSouthStageMode)]
      .contramap(g => (g.common, g.stageMode))

  implicit val arbStaticConfig: Arbitrary[StaticConfig] =
    Arbitrary(arbitrary[Instrument].flatMap(genStaticConfigOf))

  val genAcqCamDynamic  : Gen[DynamicConfig.AcqCam]   = Gen.const(DynamicConfig.AcqCam()  )
  val genBhrosDynamic   : Gen[DynamicConfig.Bhros]    = Gen.const(DynamicConfig.Bhros()   )
  val genGhostDynamic   : Gen[DynamicConfig.Ghost]    = Gen.const(DynamicConfig.Ghost()   )
  val genGpiDynamic     : Gen[DynamicConfig.Gpi]      = Gen.const(DynamicConfig.Gpi()     )
  val genGsaoiDynamic   : Gen[DynamicConfig.Gsaoi]    = Gen.const(DynamicConfig.Gsaoi()   )
  val genMichelleDynamic: Gen[DynamicConfig.Michelle] = Gen.const(DynamicConfig.Michelle())
  val genNiciDynamic    : Gen[DynamicConfig.Nici]     = Gen.const(DynamicConfig.Nici()    )
  val genNifsDynamic    : Gen[DynamicConfig.Nifs]     = Gen.const(DynamicConfig.Nifs()    )
  val genNiriDynamic    : Gen[DynamicConfig.Niri]     = Gen.const(DynamicConfig.Niri()    )
  val genPhoenixDynamic : Gen[DynamicConfig.Phoenix]  = Gen.const(DynamicConfig.Phoenix() )
  val genTrecsDynamic   : Gen[DynamicConfig.Trecs]    = Gen.const(DynamicConfig.Trecs()   )
  val genVisitorDynamic : Gen[DynamicConfig.Visitor]  = Gen.const(DynamicConfig.Visitor() )

  implicit val arbF2FpuChoice      =
    Arbitrary {
      Gen.oneOf(Gen.const(F2FpuChoice.Custom),
                arbitrary[F2Fpu].map(F2FpuChoice.Builtin(_)))
    }

  val genF2Dynamic: Gen[DynamicConfig.Flamingos2] =
    for {
      d <- arbitrary[Option[F2Disperser]]
      e <- arbitrary[Duration           ]
      f <- arbitrary[F2Filter           ]
      u <- arbitrary[Option[F2FpuChoice]]
      l <- arbitrary[F2LyotWheel        ]
      r <- arbitrary[F2ReadMode         ]
      w <- arbitrary[F2WindowCover      ]
    } yield DynamicConfig.Flamingos2(d, e, f, u, l, r, w)

  implicit val arbGmosCcdReadout =
    Arbitrary {
      for {
        x <- arbitrary[GmosXBinning]
        y <- arbitrary[GmosYBinning]
        c <- arbitrary[GmosAmpCount]
        g <- arbitrary[GmosAmpGain]
        r <- arbitrary[GmosAmpReadMode]
      } yield GmosConfig.GmosCcdReadout(x, y, c, g, r)
    }

  implicit val cogGmosCcdReadout: Cogen[GmosConfig.GmosCcdReadout] =
    Cogen[(GmosXBinning, GmosYBinning, GmosAmpCount, GmosAmpGain, GmosAmpReadMode)]
      .contramap(c => (c.xBinning, c.yBinning, c.ampCount, c.ampGain, c.ampReadMode))

  implicit val arbGmosCommonDynamic =
    Arbitrary {
      for {
        c <- arbitrary[GmosConfig.GmosCcdReadout]
        d <- arbitrary[GmosDtax]
        e <- arbitrary[Duration]
        r <- arbitrary[GmosRoi]
      } yield GmosConfig.GmosCommonDynamicConfig(c, d, e, r)
    }

  implicit val cogGmosCommonDynamic: Cogen[GmosConfig.GmosCommonDynamicConfig] =
    Cogen[(GmosCcdReadout, GmosDtax, Duration, GmosRoi)]
      .contramap(c => (c.ccdReadout, c.dtaxOffset, c.exposureTime, c.roi))

  implicit val arbGmosCustomMask =
    Arbitrary {
      for {
        m <- Gen.alphaStr.map(_.take(32))
        w <- arbitrary[GmosCustomSlitWidth]
      } yield GmosConfig.GmosCustomMask(m, w)
    }

  implicit val cogGmosCustomMask: Cogen[GmosConfig.GmosCustomMask] =
    Cogen[(String, GmosCustomSlitWidth)].contramap(g => (g.maskDefinitionFilename, g.slitWidth))

  implicit val arbGmosNorthGrating =
    Arbitrary {
      for {
        d <- arbitrary[GmosNorthDisperser]
        o <- arbitrary[GmosDisperserOrder]
        w <- Gen.choose(3000, 12000).map(Wavelength.fromAngstroms.unsafeGet)
      } yield GmosConfig.GmosGrating(d, o, w)
    }

  implicit val cogGmosNorthGrating: Cogen[GmosConfig.GmosGrating[GmosNorthDisperser]] =
    Cogen[(GmosNorthDisperser, GmosDisperserOrder, Wavelength)]
      .contramap(g => (g.disperser, g.order, g.wavelength))

  implicit val arbGmosSouthGrating =
    Arbitrary {
      for {
        d <- arbitrary[GmosSouthDisperser]
        o <- arbitrary[GmosDisperserOrder]
        w <- Gen.choose(3000, 12000).map(Wavelength.fromAngstroms.unsafeGet)
      } yield GmosConfig.GmosGrating(d, o, w)
    }

  implicit val cogGmosSouthGrating: Cogen[GmosConfig.GmosGrating[GmosSouthDisperser]] =
    Cogen[(GmosSouthDisperser, GmosDisperserOrder, Wavelength)]
      .contramap(g => (g.disperser, g.order, g.wavelength))

  val genGmosNorthDynamic: Gen[DynamicConfig.GmosN] =
    for {
      c <- arbitrary[GmosConfig.GmosCommonDynamicConfig]
      g <- arbitrary[Option[GmosConfig.GmosGrating[GmosNorthDisperser]]]
      f <- arbitrary[Option[GmosNorthFilter]]
      u <- arbitrary[Option[Either[GmosCustomMask, GmosNorthFpu]]]
    } yield DynamicConfig.GmosN(c, g, f, u)

  val genGmosSouthDynamic: Gen[DynamicConfig.GmosS] =
    for {
      c <- arbitrary[GmosConfig.GmosCommonDynamicConfig]
      g <- arbitrary[Option[GmosConfig.GmosGrating[GmosSouthDisperser]]]
      f <- arbitrary[Option[GmosSouthFilter]]
      u <- arbitrary[Option[Either[GmosCustomMask, GmosSouthFpu]]]
    } yield DynamicConfig.GmosS(c, g, f, u)

  val genGnirsDynamic: Gen[DynamicConfig.Gnirs] =
      for {
        a <- arbitrary[GnirsAcquisitionMirror             ]
        b <- arbitrary[GnirsCamera                        ]
        c <- arbitrary[CoAdds                             ]
        d <- arbitrary[GnirsDecker                        ]
        e <- arbitrary[GnirsDisperser                     ]
        f <- arbitrary[Duration                           ]
        g <- arbitrary[GnirsFilter                        ]
        h <- arbitrary[Either[GnirsFpuOther, GnirsFpuSlit]]
        i <- arbitrary[GnirsPrism                         ]
        j <- arbitrary[GnirsReadMode                      ]
        k <- Gen.choose(1000, 120000).map(Wavelength.fromAngstroms.unsafeGet)
      } yield DynamicConfig.Gnirs(a, b, c, d, e, f, g, h, i, j, k)

  def genDynamicConfigOf(i: Instrument): Gen[DynamicConfig] = {
    i match {
      case AcqCam     => genAcqCamDynamic
      case Bhros      => genBhrosDynamic
      case Flamingos2 => genF2Dynamic
      case Ghost      => genGhostDynamic
      case GmosN      => genGmosNorthDynamic
      case GmosS      => genGmosSouthDynamic
      case Gnirs      => genGnirsDynamic
      case Gpi        => genGpiDynamic
      case Gsaoi      => genGsaoiDynamic
      case Michelle   => genMichelleDynamic
      case Nici       => genNiciDynamic
      case Nifs       => genNifsDynamic
      case Niri       => genNiriDynamic
      case Phoenix    => genPhoenixDynamic
      case Trecs      => genTrecsDynamic
      case Visitor    => genVisitorDynamic
    }
  }

  implicit val arbGmosNDynamicConfig: Arbitrary[DynamicConfig.GmosN] =
    Arbitrary(genGmosNorthDynamic)

  implicit val cogGmosNDynamicConfig: Cogen[DynamicConfig.GmosN] =
    Cogen[(GmosConfig.GmosCommonDynamicConfig, Option[GmosConfig.GmosGrating[GmosNorthDisperser]], Option[GmosNorthFilter], Option[Either[GmosConfig.GmosCustomMask, GmosNorthFpu]])]
      .contramap(g => (g.common, g.grating, g.filter, g.fpu))

  implicit val arbGmosSDynamicConfig: Arbitrary[DynamicConfig.GmosS] =
    Arbitrary(genGmosSouthDynamic)

  implicit val cogGmosSDynamicConfig: Cogen[DynamicConfig.GmosS] =
    Cogen[(GmosConfig.GmosCommonDynamicConfig, Option[GmosConfig.GmosGrating[GmosSouthDisperser]], Option[GmosSouthFilter], Option[Either[GmosConfig.GmosCustomMask, GmosSouthFpu]])]
      .contramap(g => (g.common, g.grating, g.filter, g.fpu))

  implicit val arbDynamicConfig: Arbitrary[DynamicConfig] =
    Arbitrary(arbitrary[Instrument].flatMap(genDynamicConfigOf))

}
