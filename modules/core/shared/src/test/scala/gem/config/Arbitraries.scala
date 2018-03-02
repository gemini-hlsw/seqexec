// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import cats._

import gem.arb._
import gem.config.F2Config.F2FpuChoice
import gem.config.GcalConfig.{GcalArcs, GcalLamp}
import gem.config.GmosConfig._
import gem.enum._
import gem.enum.Instrument._
import gem.math.{ Offset, Wavelength }

import org.scalacheck._
import org.scalacheck.Arbitrary._

import java.time.Duration

trait Arbitraries {
  import ArbEnumerated._
  import ArbOffset._


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

  implicit val arbDuration: Arbitrary[Duration] =
    Arbitrary(Gen.posNum[Long].map(Duration.ofMillis))

  implicit val arbMosPreImaging: Arbitrary[MosPreImaging] =
    Arbitrary(
      Gen.oneOf(MosPreImaging.IsMosPreImaging,
                MosPreImaging.IsNotMosPreImaging)
    )

  val genAcqCamStatic:   Gen[StaticConfig.Aux[AcqCam.type]]    = Gen.const(StaticConfig.AcqCam()  )
  val genBhrosStatic:    Gen[StaticConfig.Aux[Bhros.type]]     = Gen.const(StaticConfig.Bhros()   )
  val genGhostStatic:    Gen[StaticConfig.Aux[Ghost.type]]     = Gen.const(StaticConfig.Ghost()   )
  val genGpiStatic:      Gen[StaticConfig.Aux[Gpi.type]]       = Gen.const(StaticConfig.Gpi()     )
  val genGsaoiStatic:    Gen[StaticConfig.Aux[Gsaoi.type]]     = Gen.const(StaticConfig.Gsaoi()   )
  val genMichelleStatic: Gen[StaticConfig.Aux[Michelle.type]]  = Gen.const(StaticConfig.Michelle())
  val genNiciStatic:     Gen[StaticConfig.Aux[Nici.type]]      = Gen.const(StaticConfig.Nici()    )
  val genNifsStatic:     Gen[StaticConfig.Aux[Nifs.type]]      = Gen.const(StaticConfig.Nifs()    )
  val genNiriStatic:     Gen[StaticConfig.Aux[Niri.type]]      = Gen.const(StaticConfig.Niri()    )
  val genPhoenixStatic:  Gen[StaticConfig.Aux[Phoenix.type]]   = Gen.const(StaticConfig.Phoenix() )
  val genTrecsStatic:    Gen[StaticConfig.Aux[Trecs.type]]     = Gen.const(StaticConfig.Trecs()   )
  val genVisitorStatic:  Gen[StaticConfig.Aux[Visitor.type]]   = Gen.const(StaticConfig.Visitor() )

  val genF2Static: Gen[StaticConfig.Aux[Flamingos2.type]] =
    arbitrary[MosPreImaging].map(StaticConfig.F2(_))

  implicit val arbGmosShuffleOffset =
    Arbitrary(Gen.posNum[Int].map(GmosConfig.GmosShuffleOffset.unsafeFromRowCount))

  implicit val arbGmosShuffleCycles =
    Arbitrary(Gen.posNum[Int].map(GmosConfig.GmosShuffleCycles.unsafeFromCycleCount))

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

  implicit val arbGmosCustomRoiEntry =
    Arbitrary(
      for {
        xMin <- Gen.posNum[Short]
        yMin <- Gen.posNum[Short]
        xRng <- Gen.posNum[Short]
        yRng <- Gen.posNum[Short]
      } yield GmosConfig.GmosCustomRoiEntry.unsafeFromDescription(xMin, yMin, xRng, yRng)
    )

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

  val genGmosNorthStatic: Gen[StaticConfig.Aux[GmosN.type]] =
    for {
      c <- arbitrary[GmosConfig.GmosCommonStaticConfig]
      s <- arbitrary[GmosNorthStageMode]
    } yield StaticConfig.GmosNorth(c, s)

  val genGmosSouthStatic: Gen[StaticConfig.Aux[GmosS.type]] =
    for {
      c <- arbitrary[GmosConfig.GmosCommonStaticConfig]
      s <- arbitrary[GmosSouthStageMode]
    } yield StaticConfig.GmosSouth(c, s)

  val genGnirsStatic: Gen[StaticConfig.Aux[Gnirs.type]] =
    arbitrary[GnirsWellDepth].map(StaticConfig.Gnirs(_))

  def genStaticConfigOf[I <: Instrument with Singleton](i: Instrument.Aux[I]): Gen[StaticConfig.Aux[I]] = {
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

  val genAcqCamDynamic  : Gen[DynamicConfig.Aux[AcqCam.type]]   = Gen.const(DynamicConfig.AcqCam()  )
  val genBhrosDynamic   : Gen[DynamicConfig.Aux[Bhros.type]]    = Gen.const(DynamicConfig.Bhros()   )
  val genGhostDynamic   : Gen[DynamicConfig.Aux[Ghost.type]]    = Gen.const(DynamicConfig.Ghost()   )
  val genGpiDynamic     : Gen[DynamicConfig.Aux[Gpi.type]]      = Gen.const(DynamicConfig.Gpi()     )
  val genGsaoiDynamic   : Gen[DynamicConfig.Aux[Gsaoi.type]]    = Gen.const(DynamicConfig.Gsaoi()   )
  val genMichelleDynamic: Gen[DynamicConfig.Aux[Michelle.type]] = Gen.const(DynamicConfig.Michelle())
  val genNiciDynamic    : Gen[DynamicConfig.Aux[Nici.type]]     = Gen.const(DynamicConfig.Nici()    )
  val genNifsDynamic    : Gen[DynamicConfig.Aux[Nifs.type]]     = Gen.const(DynamicConfig.Nifs()    )
  val genNiriDynamic    : Gen[DynamicConfig.Aux[Niri.type]]     = Gen.const(DynamicConfig.Niri()    )
  val genPhoenixDynamic : Gen[DynamicConfig.Aux[Phoenix.type]]  = Gen.const(DynamicConfig.Phoenix() )
  val genTrecsDynamic   : Gen[DynamicConfig.Aux[Trecs.type]]    = Gen.const(DynamicConfig.Trecs()   )
  val genVisitorDynamic : Gen[DynamicConfig.Aux[Visitor.type]]  = Gen.const(DynamicConfig.Visitor() )

  implicit val arbF2FpuChoice      =
    Arbitrary {
      Gen.oneOf(Gen.const(F2FpuChoice.Custom),
                arbitrary[F2Fpu].map(F2FpuChoice.Builtin(_)))
    }

  val genF2Dynamic: Gen[DynamicConfig.Aux[Flamingos2.type]] =
    for {
      d <- arbitrary[Option[F2Disperser]]
      e <- arbitrary[Duration           ]
      f <- arbitrary[F2Filter           ]
      u <- arbitrary[Option[F2FpuChoice]]
      l <- arbitrary[F2LyotWheel        ]
      r <- arbitrary[F2ReadMode         ]
      w <- arbitrary[F2WindowCover      ]
    } yield DynamicConfig.F2(d, e, f, u, l, r, w)

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

  implicit val arbGmosCommonDynamic =
    Arbitrary {
      for {
        c <- arbitrary[GmosConfig.GmosCcdReadout]
        d <- arbitrary[GmosDtax]
        e <- arbitrary[Duration]
        r <- arbitrary[GmosRoi]
      } yield GmosConfig.GmosCommonDynamicConfig(c, d, e, r)
    }

  implicit val arbGmosCustomMask =
    Arbitrary {
      for {
        m <- Gen.alphaStr.map(_.take(32))
        w <- arbitrary[GmosCustomSlitWidth]
      } yield GmosConfig.GmosCustomMask(m, w)
    }

  implicit val arbGmosNorthGrating =
    Arbitrary {
      for {
        d <- arbitrary[GmosNorthDisperser]
        o <- arbitrary[GmosDisperserOrder]
        w <- Gen.choose(3000, 12000).map(Wavelength.fromAngstroms.unsafeGet)
      } yield GmosConfig.GmosGrating(d, o, w)
    }

  implicit val arbGmosSouthGrating =
    Arbitrary {
      for {
        d <- arbitrary[GmosSouthDisperser]
        o <- arbitrary[GmosDisperserOrder]
        w <- Gen.choose(3000, 12000).map(Wavelength.fromAngstroms.unsafeGet)
      } yield GmosConfig.GmosGrating(d, o, w)
    }

  val genGmosNorthDynamic: Gen[DynamicConfig.Aux[GmosN.type]] =
    for {
      c <- arbitrary[GmosConfig.GmosCommonDynamicConfig]
      g <- arbitrary[Option[GmosConfig.GmosGrating[GmosNorthDisperser]]]
      f <- arbitrary[Option[GmosNorthFilter]]
      u <- arbitrary[Option[Either[GmosCustomMask, GmosNorthFpu]]]
    } yield DynamicConfig.GmosNorth(c, g, f, u)

  val genGmosSouthDynamic: Gen[DynamicConfig.Aux[GmosS.type]] =
    for {
      c <- arbitrary[GmosConfig.GmosCommonDynamicConfig]
      g <- arbitrary[Option[GmosConfig.GmosGrating[GmosSouthDisperser]]]
      f <- arbitrary[Option[GmosSouthFilter]]
      u <- arbitrary[Option[Either[GmosCustomMask, GmosSouthFpu]]]
    } yield DynamicConfig.GmosSouth(c, g, f, u)

  val genGnirsDynamic: Gen[DynamicConfig.Aux[Gnirs.type]] =
      for {
        a <- arbitrary[GnirsCamera                        ]
        b <- arbitrary[GnirsDecker                        ]
        c <- arbitrary[GnirsDisperser                     ]
        d <- arbitrary[Duration                           ]
        e <- arbitrary[GnirsFilter                        ]
        f <- arbitrary[Either[GnirsFpuOther, GnirsFpuSlit]]
        g <- arbitrary[GnirsPrism                         ]
        h <- arbitrary[GnirsReadMode                      ]
        i <- Gen.choose(1000, 120000).map(Wavelength.fromAngstroms.unsafeGet)
      } yield DynamicConfig.Gnirs(a, b, c, d, e, f, g, h, i)

  def genDynamicConfigOf[I <: Instrument with Singleton](i: Instrument.Aux[I]): Gen[DynamicConfig.Aux[I]] = {
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

  // GcalConfig

  implicit val arbGcalArcs: Arbitrary[GcalArcs] =
    Arbitrary {
      for {
        a  <- arbitrary[GcalArc]
        as <- Gen.someOf(GcalArc.all)
      } yield GcalArcs(a, as.toList)
    }

  implicit val arbGcalLamp: Arbitrary[GcalLamp] =
    Arbitrary(Gen.oneOf(
      arbitrary[GcalContinuum].map(Left(_)),
      arbitrary[GcalArcs     ].map(Right(_))
    ))

  implicit val arbGcalConfig: Arbitrary[GcalConfig] =
    Arbitrary {
      for {
        l <- arbitrary[GcalLamp    ]
        f <- arbitrary[GcalFilter  ]
        d <- arbitrary[GcalDiffuser]
        s <- arbitrary[GcalShutter ]
        e <- arbitrary[Duration    ]
        c <- Gen.posNum[Short]
      } yield GcalConfig(l, f, d, s, e, c)
    }
}
