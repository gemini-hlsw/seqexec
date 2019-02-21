// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import cats._

import gem.CoAdds
import gem.arb._
import gem.enum._
import gem.enum.Instrument._
import gem.math.Wavelength

import org.scalacheck._
import org.scalacheck.Arbitrary._

import java.time.Duration

trait Arbitraries {

  import ArbCoAdds._
  import ArbEnumerated._
  import ArbFlamingos2._
  import ArbGmos._
  import ArbTime._


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

  val genGnirsStatic: Gen[StaticConfig.Gnirs] =
    arbitrary[GnirsWellDepth].map(StaticConfig.Gnirs(_))

  def genStaticConfigOf(i: Instrument): Gen[StaticConfig] = {
    i match {
      case AcqCam     => genAcqCamStatic
      case Bhros      => genBhrosStatic
      case Flamingos2 => arbitrary[StaticConfig.Flamingos2]
      case Ghost      => genGhostStatic
      case GmosN      => arbitrary[StaticConfig.GmosN]
      case GmosS      => arbitrary[StaticConfig.GmosS]
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
      case Flamingos2 => arbitrary[DynamicConfig.Flamingos2]
      case Ghost      => genGhostDynamic
      case GmosN      => arbitrary[DynamicConfig.GmosN]
      case GmosS      => arbitrary[DynamicConfig.GmosS]
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

  implicit val arbDynamicConfig: Arbitrary[DynamicConfig] =
    Arbitrary(arbitrary[Instrument].flatMap(genDynamicConfigOf))

}
