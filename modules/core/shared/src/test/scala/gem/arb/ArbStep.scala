// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.{Instrument, SmartGcalType}
import gem.config.{DynamicConfig, GcalConfig, TelescopeConfig}
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbStep {

  import ArbEnumerated._
  import ArbGcalConfig._
  import ArbGmos._
  import ArbDynamicConfig._
  import ArbTelescopeConfig._

  import Step.Base._

  implicit val arbStepBaseBias: Arbitrary[Bias.type] =
    Arbitrary(Gen.const(Bias))

  implicit val cogStepBaseBias: Cogen[Bias.type] =
    Cogen[Unit].contramap(Function.const(()))

  implicit val arbStepBaseDark: Arbitrary[Dark.type] =
    Arbitrary(Gen.const(Dark))

  implicit val cogStepBaseDark: Cogen[Dark.type] =
    Cogen[Unit].contramap(Function.const(()))

  implicit val arbStepBaseGcal: Arbitrary[Gcal] =
    Arbitrary(arbitrary[GcalConfig].map(Gcal(_)))

  implicit val cogStepBaseGcal: Cogen[Gcal] =
    Cogen[GcalConfig].contramap(_.gcal)

  implicit val arbStepBaseScience: Arbitrary[Science] =
    Arbitrary(arbitrary[TelescopeConfig].map(Science(_)))

  implicit val cogStepBaseScience: Cogen[Science] =
    Cogen[TelescopeConfig].contramap(_.telescope)

  implicit val arbStepBaseSmartGcal: Arbitrary[SmartGcal] =
    Arbitrary(arbitrary[SmartGcalType].map(SmartGcal(_)))

  implicit val cogStepBaseSmartGcal: Cogen[SmartGcal] =
    Cogen[SmartGcalType].contramap(_.smartGcalType)

  implicit val arbStepBase: Arbitrary[Step.Base] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[Bias.type],
        arbitrary[Dark.type],
        arbitrary[Gcal     ],
        arbitrary[Science  ],
        arbitrary[SmartGcal]
      )
    }

  implicit val cogStepBase: Cogen[Step.Base] =
    Cogen[(Option[Bias.type], Option[Dark.type], Option[Gcal], Option[Science], Option[SmartGcal])]
      .contramap {
        case Bias         => (Some(Bias), None,       None,    None,    None   )
        case Dark         => (None,       Some(Dark), None,    None,    None   )
        case b: Gcal      => (None,       None,       Some(b), None,    None   )
        case b: Science   => (None,       None,       None,    Some(b), None   )
        case b: SmartGcal => (None,       None,       None,    None,    Some(b))
      }

  def genStepOf(i: Instrument): Gen[Step] =
    for {
      b <- arbitrary[Step.Base]
      d <- genDynamicConfigOf(i)
    } yield d.toStep(b)

  implicit val arbStep: Arbitrary[Step] =
    Arbitrary(arbitrary[Instrument].flatMap(genStepOf))

  implicit val arbStepGmosN: Arbitrary[Step.GmosN] =
    Arbitrary {
      for {
        d <- arbitrary[DynamicConfig.GmosN]
        b <- arbitrary[Step.Base]
      } yield Step.GmosN(d, b)
    }

  implicit val cogStepGmosN: Cogen[Step.GmosN] =
    Cogen[(DynamicConfig.GmosN, Step.Base)].contramap(g => (g.dynamicConfig, g.base))

  implicit val arbStepGmosS: Arbitrary[Step.GmosS] =
    Arbitrary {
      for {
        d <- arbitrary[DynamicConfig.GmosS]
        b <- arbitrary[Step.Base]
      } yield Step.GmosS(d, b)
    }

  implicit val cogStepGmosS: Cogen[Step.GmosS] =
    Cogen[(DynamicConfig.GmosS, Step.Base)].contramap(g => (g.dynamicConfig, g.base))


  def genSequenceOf(i: Instrument): Gen[List[Step]] =
    for {
      n <- Gen.choose(0, 50)
      s <- Gen.listOfN(n, genStepOf(i))
    } yield s

}
object ArbStep extends ArbStep
