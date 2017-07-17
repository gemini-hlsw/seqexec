// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.arb._
import gem.config.GcalConfig.{GcalArcs, GcalLamp}
import gem.enum._
import gem.enum.Instrument._

import org.scalacheck._
import org.scalacheck.Arbitrary._

import java.time.Duration

import scalaz._, Scalaz._


trait Arbitraries {
  import ArbEnumerated._
  import ArbDisjunction._


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

  implicit val arbGmosNorthStatic =
    Arbitrary(
      for {
        d <- arbitrary[GmosDetector]
        p <- arbitrary[MosPreImaging]
        s <- arbitrary[GmosNorthStageMode]
      } yield StaticConfig.GmosNorth(Gmos.GmosCommonStaticConfig(d, p, None), s)
    )

  implicit val arbGmosSouthStatic =
    Arbitrary(
      for {
        d <- arbitrary[GmosDetector]
        p <- arbitrary[MosPreImaging]
        s <- arbitrary[GmosSouthStageMode]
      } yield StaticConfig.GmosSouth(Gmos.GmosCommonStaticConfig(d, p, None), s)
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
      } yield DynamicConfig.GmosNorth(c, g, f, u)
    }

  implicit val arbGmosSouthDynamic =
    Arbitrary {
      for {
        c <- arbitrary[Gmos.GmosCommonDynamicConfig]
        g <- arbitrary[Option[Gmos.GmosGrating[GmosSouthDisperser]]]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[Option[Gmos.GmosCustomMask \/ GmosSouthFpu]]
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
