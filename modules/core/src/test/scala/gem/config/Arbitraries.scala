// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import cats._

import gem.arb._
import gem.config.F2Config.F2FpuChoice
import gem.config.GcalConfig.{GcalArcs, GcalLamp}
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

  private def const[A](a: A): Arbitrary[A] =
    Arbitrary(Gen.const(a))

  implicit val arbMosPreImaging: Arbitrary[MosPreImaging] =
    Arbitrary(
      Gen.oneOf(MosPreImaging.IsMosPreImaging,
                MosPreImaging.IsNotMosPreImaging)
    )

  implicit val arbAcqCamStatic    = const(StaticConfig.AcqCam()         )
  implicit val arbBhrosStatic     = const(StaticConfig.Bhros()          )
  implicit val arbGnirsStatic     = const(StaticConfig.Gnirs()          )
  implicit val arbGpiStatic       = const(StaticConfig.Gpi()            )
  implicit val arbGsaoiStatic     = const(StaticConfig.Gsaoi()          )
  implicit val arbMichelleStatic  = const(StaticConfig.Michelle()       )
  implicit val arbNiciStatic      = const(StaticConfig.Nici()           )
  implicit val arbNifsStatic      = const(StaticConfig.Nifs()           )
  implicit val arbNiriStatic      = const(StaticConfig.Niri()           )
  implicit val arbPhoenixStatic   = const(StaticConfig.Phoenix()        )
  implicit val arbTrecsStatic     = const(StaticConfig.Trecs()          )
  implicit val arbVisitorStatic   = const(StaticConfig.Visitor()        )

  implicit val arbF2Static        =
    Arbitrary(arbitrary[MosPreImaging].map(StaticConfig.F2(_)))

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

  implicit val arbGmosNorthStatic =
    Arbitrary(
      for {
        c <- arbitrary[GmosConfig.GmosCommonStaticConfig]
        s <- arbitrary[GmosNorthStageMode]
      } yield StaticConfig.GmosNorth(c, s)
    )

  implicit val arbGmosSouthStatic =
    Arbitrary(
      for {
        c <- arbitrary[GmosConfig.GmosCommonStaticConfig]
        s <- arbitrary[GmosSouthStageMode]
      } yield StaticConfig.GmosSouth(c, s)
    )

  def genStaticConfigOf(i: Instrument): Gen[StaticConfig] =
    i match {
      case AcqCam     => arbitrary[StaticConfig.AcqCam   ]
      case Bhros      => arbitrary[StaticConfig.Bhros    ]
      case Flamingos2 => arbitrary[StaticConfig.F2       ]
      case GmosN      => arbitrary[StaticConfig.GmosNorth]
      case GmosS      => arbitrary[StaticConfig.GmosSouth]
      case Gnirs      => arbitrary[StaticConfig.Gnirs    ]
      case Gpi        => arbitrary[StaticConfig.Gpi      ]
      case Gsaoi      => arbitrary[StaticConfig.Gsaoi    ]
      case Michelle   => arbitrary[StaticConfig.Michelle ]
      case Nici       => arbitrary[StaticConfig.Nici     ]
      case Nifs       => arbitrary[StaticConfig.Nifs     ]
      case Niri       => arbitrary[StaticConfig.Niri     ]
      case Phoenix    => arbitrary[StaticConfig.Phoenix  ]
      case Trecs      => arbitrary[StaticConfig.Trecs    ]
      case Visitor    => arbitrary[StaticConfig.Visitor  ]
    }

  implicit val arbAcqCamDynamic    = const(DynamicConfig.AcqCam()         )
  implicit val arbBhrosDynamic     = const(DynamicConfig.Bhros()          )
  implicit val arbGnirsDynamic     = const(DynamicConfig.Gnirs()          )
  implicit val arbGpiDynamic       = const(DynamicConfig.Gpi()            )
  implicit val arbGsaoiDynamic     = const(DynamicConfig.Gsaoi()          )
  implicit val arbMichelleDynamic  = const(DynamicConfig.Michelle()       )
  implicit val arbNiciDynamic      = const(DynamicConfig.Nici()           )
  implicit val arbNifsDynamic      = const(DynamicConfig.Nifs()           )
  implicit val arbNiriDynamic      = const(DynamicConfig.Niri()           )
  implicit val arbPhoenixDynamic   = const(DynamicConfig.Phoenix()        )
  implicit val arbTrecsDynamic     = const(DynamicConfig.Trecs()          )
  implicit val arbVisitorDynamic   = const(DynamicConfig.Visitor()        )

  implicit val arbF2FpuChoice      =
    Arbitrary {
      Gen.oneOf(Gen.const(F2FpuChoice.Custom),
                arbitrary[F2Fpu].map(F2FpuChoice.Builtin(_)))
    }

  implicit val arbF2Dynamic       =
    Arbitrary {
      for {
        d <- arbitrary[Option[F2Disperser]]
        e <- arbitrary[Duration           ]
        f <- arbitrary[F2Filter           ]
        u <- arbitrary[Option[F2FpuChoice]]
        l <- arbitrary[F2LyotWheel        ]
        r <- arbitrary[F2ReadMode         ]
        w <- arbitrary[F2WindowCover      ]
      } yield DynamicConfig.F2(d, e, f, u, l, r, w)
    }

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
        w <- Gen.choose(3000, 12000).map(Wavelength.unsafeFromAngstroms)
      } yield GmosConfig.GmosGrating(d, o, w)
    }

  implicit val arbGmosSouthGrating =
    Arbitrary {
      for {
        d <- arbitrary[GmosSouthDisperser]
        o <- arbitrary[GmosDisperserOrder]
        w <- Gen.choose(3000, 12000).map(Wavelength.unsafeFromAngstroms)
      } yield GmosConfig.GmosGrating(d, o, w)
    }

  implicit val arbGmosNorthDynamic =
    Arbitrary {
      for {
        c <- arbitrary[GmosConfig.GmosCommonDynamicConfig]
        g <- arbitrary[Option[GmosConfig.GmosGrating[GmosNorthDisperser]]]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[Option[Either[Gmos.GmosCustomMask, GmosNorthFpu]]]
      } yield DynamicConfig.GmosNorth(c, g, f, u)
    }

  implicit val arbGmosSouthDynamic =
    Arbitrary {
      for {
        c <- arbitrary[GmosConfig.GmosCommonDynamicConfig]
        g <- arbitrary[Option[GmosConfig.GmosGrating[GmosSouthDisperser]]]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[Option[Either[Gmos.GmosCustomMask, GmosSouthFpu]]]
      } yield DynamicConfig.GmosSouth(c, g, f, u)
    }

  def genDynamicConfigOf(i: Instrument): Gen[DynamicConfig] =
    i match {
      case AcqCam     => arbitrary[DynamicConfig.AcqCam   ]
      case Bhros      => arbitrary[DynamicConfig.Bhros    ]
      case Flamingos2 => arbitrary[DynamicConfig.F2       ]
      case GmosN      => arbitrary[DynamicConfig.GmosNorth]
      case GmosS      => arbitrary[DynamicConfig.GmosSouth]
      case Gnirs      => arbitrary[DynamicConfig.Gnirs    ]
      case Gpi        => arbitrary[DynamicConfig.Gpi      ]
      case Gsaoi      => arbitrary[DynamicConfig.Gsaoi    ]
      case Michelle   => arbitrary[DynamicConfig.Michelle ]
      case Nici       => arbitrary[DynamicConfig.Nici     ]
      case Nifs       => arbitrary[DynamicConfig.Nifs     ]
      case Niri       => arbitrary[DynamicConfig.Niri     ]
      case Phoenix    => arbitrary[DynamicConfig.Phoenix  ]
      case Trecs      => arbitrary[DynamicConfig.Trecs    ]
      case Visitor    => arbitrary[DynamicConfig.Visitor  ]
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
