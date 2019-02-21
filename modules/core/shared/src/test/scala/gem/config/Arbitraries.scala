// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.arb._
import gem.enum._
import gem.enum.Instrument._

import org.scalacheck._
import org.scalacheck.Arbitrary._


trait Arbitraries {

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

 def genDynamicConfigOf(i: Instrument): Gen[DynamicConfig] = {
    i match {
      case AcqCam     => genAcqCamDynamic
      case Bhros      => genBhrosDynamic
      case Flamingos2 => arbitrary[DynamicConfig.Flamingos2]
      case Ghost      => genGhostDynamic
      case GmosN      => arbitrary[DynamicConfig.GmosN]
      case GmosS      => arbitrary[DynamicConfig.GmosS]
      case Gnirs      => arbitrary[DynamicConfig.Gnirs]
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
