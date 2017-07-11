// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.config.GcalConfig.{GcalArcs, GcalLamp}
import gem.enum._
import gem.enum.Instrument._

import org.scalacheck._
import org.scalacheck.Arbitrary._

import java.time.Duration

import scalaz._, Scalaz._


trait Arbitraries {
  import gem.arb.ArbEnumerated._

  // Surely this is already defined somewhere?
  implicit val functorGen = new Functor[Gen] {
    def map[A, B](fa: Gen[A])(f: A => B): Gen[B] =
      fa.map(f)
  }

  implicit val applicativeGen = new Applicative[Gen] {
    def ap[A, B](ga: => Gen[A])(gf: => Gen[(A) => B]): Gen[B] =
      for {
        f <- gf
        a <- ga
      } yield f(a)

    def point[A](a: => A): Gen[A] =
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

  implicit val arbAcqCamStatic    = const(AcqCamStaticConfig()         )
  implicit val arbBhrosStatic     = const(BhrosStaticConfig()          )
  implicit val arbGnirsStatic     = const(GnirsStaticConfig()          )
  implicit val arbGpiStatic       = const(GpiStaticConfig()            )
  implicit val arbGsaoiStatic     = const(GsaoiStaticConfig()          )
  implicit val arbMichelleStatic  = const(MichelleStaticConfig()       )
  implicit val arbNiciStatic      = const(NiciStaticConfig()           )
  implicit val arbNifsStatic      = const(NifsStaticConfig()           )
  implicit val arbNiriStatic      = const(NiriStaticConfig()           )
  implicit val arbPhoenixStatic   = const(PhoenixStaticConfig()        )
  implicit val arbTrecsStatic     = const(TrecsStaticConfig()          )
  implicit val arbVisitorStatic   = const(VisitorStaticConfig()        )

  implicit val arbF2Static        =
    Arbitrary(arbitrary[MosPreImaging].map(F2StaticConfig(_)))

  implicit val arbGmosNorthStatic =
    Arbitrary(
      for {
        d <- arbitrary[GmosDetector]
        p <- arbitrary[MosPreImaging]
        s <- arbitrary[GmosNorthStageMode]
      } yield GmosNorthStaticConfig(Gmos.GmosCommonStaticConfig(d, p, None), s)
    )

  implicit val arbGmosSouthStatic =
    Arbitrary(
      for {
        d <- arbitrary[GmosDetector]
        p <- arbitrary[MosPreImaging]
        s <- arbitrary[GmosSouthStageMode]
      } yield GmosSouthStaticConfig(Gmos.GmosCommonStaticConfig(d, p, None), s)
    )

  def genStaticConfigOf(i: Instrument): Gen[StaticConfig] =
    i match {
      case AcqCam     => arbitrary[AcqCamStaticConfig   ]
      case Bhros      => arbitrary[BhrosStaticConfig    ]
      case Flamingos2 => arbitrary[F2StaticConfig       ]
      case GmosN      => arbitrary[GmosNorthStaticConfig]
      case GmosS      => arbitrary[GmosSouthStaticConfig]
      case Gnirs      => arbitrary[GnirsStaticConfig    ]
      case Gpi        => arbitrary[GpiStaticConfig      ]
      case Gsaoi      => arbitrary[GsaoiStaticConfig    ]
      case Michelle   => arbitrary[MichelleStaticConfig ]
      case Nici       => arbitrary[NiciStaticConfig     ]
      case Nifs       => arbitrary[NifsStaticConfig     ]
      case Niri       => arbitrary[NiriStaticConfig     ]
      case Phoenix    => arbitrary[PhoenixStaticConfig  ]
      case Trecs      => arbitrary[TrecsStaticConfig    ]
      case Visitor    => arbitrary[VisitorStaticConfig  ]
    }

  implicit val arbAcqCamDynamic    = const(AcqCamDynamicConfig()         )
  implicit val arbBhrosDynamic     = const(BhrosDynamicConfig()          )
  implicit val arbGnirsDynamic     = const(GnirsDynamicConfig()          )
  implicit val arbGpiDynamic       = const(GpiDynamicConfig()            )
  implicit val arbGsaoiDynamic     = const(GsaoiDynamicConfig()          )
  implicit val arbMichelleDynamic  = const(MichelleDynamicConfig()       )
  implicit val arbNiciDynamic      = const(NiciDynamicConfig()           )
  implicit val arbNifsDynamic      = const(NifsDynamicConfig()           )
  implicit val arbNiriDynamic      = const(NiriDynamicConfig()           )
  implicit val arbPhoenixDynamic   = const(PhoenixDynamicConfig()        )
  implicit val arbTrecsDynamic     = const(TrecsDynamicConfig()          )
  implicit val arbVisitorDynamic   = const(VisitorDynamicConfig()        )

  implicit val arbF2Dynamic       =
    Arbitrary {
      for {
        d <- arbitrary[F2Disperser  ]
        e <- arbitrary[Duration     ]
        f <- arbitrary[F2Filter     ]
        u <- arbitrary[F2FpUnit     ]
        l <- arbitrary[F2LyotWheel  ]
        r <- arbitrary[F2ReadMode   ]
        w <- arbitrary[F2WindowCover]
      } yield F2DynamicConfig(d, e, f, u, l, r, w)
    }

  implicit val arbGmosCcdReadout =
    Arbitrary {
      for {
        x <- arbitrary[GmosXBinning]
        y <- arbitrary[GmosYBinning]
        c <- arbitrary[GmosAmpCount]
        g <- arbitrary[GmosAmpGain]
        r <- arbitrary[GmosAmpReadMode]
      } yield Gmos.GmosCcdReadout(x, y, c, g, r)
    }

  implicit val arbGmosCommonDynamic =
    Arbitrary {
      for {
        c <- arbitrary[Gmos.GmosCcdReadout]
        d <- arbitrary[GmosDtax]
        e <- arbitrary[Duration]
      } yield Gmos.GmosCommonDynamicConfig(c, d, e)
    }

  implicit val arbGmosCustomMask =
    Arbitrary {
      for {
        m <- Gen.alphaStr.map(_.take(32))
        w <- arbitrary[GmosCustomSlitWidth]
      } yield Gmos.GmosCustomMask(m, w)
    }

  implicit val arbGmosCentralWavelength =
    Arbitrary {
      Gen.const(Gmos.GmosCentralWavelength(0.0)) // for now ...
    }

  implicit val arbGmosNorthGrating =
    Arbitrary {
      for {
        d <- arbitrary[GmosNorthDisperser]
        o <- arbitrary[GmosDisperserOrder]
        w <- arbitrary[Gmos.GmosCentralWavelength]
      } yield Gmos.GmosGrating(d, o, w)
    }

  implicit val arbGmosSouthGrating =
    Arbitrary {
      for {
        d <- arbitrary[GmosSouthDisperser]
        o <- arbitrary[GmosDisperserOrder]
        w <- arbitrary[Gmos.GmosCentralWavelength]
      } yield Gmos.GmosGrating(d, o, w)
    }

  implicit val arbGmosNorthDynamic =
    Arbitrary {
      for {
        c <- arbitrary[Gmos.GmosCommonDynamicConfig]
        g <- arbitrary[Option[Gmos.GmosGrating[GmosNorthDisperser]]]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[Option[Gmos.GmosCustomMask \/ GmosNorthFpu]]
      } yield GmosNorthDynamicConfig(c, g, f, u)
    }

  implicit val arbGmosSouthDynamic =
    Arbitrary {
      for {
        c <- arbitrary[Gmos.GmosCommonDynamicConfig]
        g <- arbitrary[Option[Gmos.GmosGrating[GmosSouthDisperser]]]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[Option[Gmos.GmosCustomMask \/ GmosSouthFpu]]
      } yield GmosSouthDynamicConfig(c, g, f, u)
    }

  def genDynamicConfigOf(i: Instrument): Gen[DynamicConfig] =
    i match {
      case AcqCam     => arbitrary[AcqCamDynamicConfig   ]
      case Bhros      => arbitrary[BhrosDynamicConfig    ]
      case Flamingos2 => arbitrary[F2DynamicConfig       ]
      case GmosN      => arbitrary[GmosNorthDynamicConfig]
      case GmosS      => arbitrary[GmosSouthDynamicConfig]
      case Gnirs      => arbitrary[GnirsDynamicConfig    ]
      case Gpi        => arbitrary[GpiDynamicConfig      ]
      case Gsaoi      => arbitrary[GsaoiDynamicConfig    ]
      case Michelle   => arbitrary[MichelleDynamicConfig ]
      case Nici       => arbitrary[NiciDynamicConfig     ]
      case Nifs       => arbitrary[NifsDynamicConfig     ]
      case Niri       => arbitrary[NiriDynamicConfig     ]
      case Phoenix    => arbitrary[PhoenixDynamicConfig  ]
      case Trecs      => arbitrary[TrecsDynamicConfig    ]
      case Visitor    => arbitrary[VisitorDynamicConfig  ]
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
      arbitrary[GcalContinuum].map(_.left[GcalArcs]),
      arbitrary[GcalArcs     ].map(_.right[GcalContinuum])
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
