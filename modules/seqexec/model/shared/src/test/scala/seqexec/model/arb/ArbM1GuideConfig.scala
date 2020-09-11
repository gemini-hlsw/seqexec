// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen
import seqexec.model.enum.M1Source
import seqexec.model.M1GuideConfig
import gem.arb.ArbEnumerated.{arbEnumerated => oldArbEnumerated}
import gem.arb.ArbEnumerated.{cogEnumerated => oldCogEnumerated}
import lucuma.core.util.arb.ArbEnumerated._

trait ArbM1GuideConfig {

  implicit val arbM1GuideOn: Arbitrary[M1GuideConfig.M1GuideOn] =
    Arbitrary {
      for {
        s <- arbitrary[M1Source]
      } yield M1GuideConfig.M1GuideOn(s)
    }

  implicit val m1GuideOnCogen: Cogen[M1GuideConfig.M1GuideOn] =
    Cogen[M1Source].contramap(_.source)

  implicit val arbM1GuideConfig: Arbitrary[M1GuideConfig] =
    Arbitrary {
      for {
        off <- Gen.const(M1GuideConfig.M1GuideOff)
        on  <- arbitrary[M1GuideConfig.M1GuideOn]
        l   <- Gen.oneOf(off, on)
      } yield l
    }

  implicit val m1GuideConfigCogen: Cogen[M1GuideConfig] =
    Cogen[Option[M1GuideConfig.M1GuideOn]].contramap {
      case x: M1GuideConfig.M1GuideOn => Some(x)
      case _                          => None
    }
}

object ArbM1GuideConfig extends ArbM1GuideConfig
