// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.arb

import gem.config.StaticConfig
import gem.enum.Instrument
import gem.enum.Instrument._

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}

trait ArbStaticConfig {

  import ArbEnumerated._
  import ArbFlamingos2._
  import ArbGmos._
  import ArbGnirs._

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

  def genStaticConfigOf(i: Instrument): Gen[StaticConfig] = {
    i match {
      case AcqCam     => genAcqCamStatic
      case Bhros      => genBhrosStatic
      case Flamingos2 => arbitrary[StaticConfig.Flamingos2]
      case Ghost      => genGhostStatic
      case GmosN      => arbitrary[StaticConfig.GmosN]
      case GmosS      => arbitrary[StaticConfig.GmosS]
      case Gnirs      => arbitrary[StaticConfig.Gnirs]
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

}

object ArbStaticConfig extends ArbStaticConfig
