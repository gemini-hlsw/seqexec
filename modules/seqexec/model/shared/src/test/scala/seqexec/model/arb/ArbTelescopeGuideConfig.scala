// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import gem.arb.ArbEnumerated.{arbEnumerated => oldArbEnumerated}
import gem.arb.ArbEnumerated.{cogEnumerated => oldCogEnumerated}
import lucuma.core.util.arb.ArbEnumerated._
import seqexec.model.TelescopeGuideConfig
import seqexec.model.enum._
import seqexec.model.M1GuideConfig
import seqexec.model.M2GuideConfig
import seqexec.model.arb.ArbM1GuideConfig._
import seqexec.model.arb.ArbM2GuideConfig._

trait ArbTelescopeGuideConfig {

  implicit val arbTelescopeGuideOn: Arbitrary[TelescopeGuideConfig] =
    Arbitrary {
      for {
        mo <- arbitrary[MountGuideOption]
        m1 <- arbitrary[M1GuideConfig]
        m2 <- arbitrary[M2GuideConfig]
      } yield TelescopeGuideConfig(mo, m1, m2)
    }

  implicit val telescopeConfigCogen: Cogen[TelescopeGuideConfig] =
    Cogen[(MountGuideOption, M1GuideConfig, M2GuideConfig)]
      .contramap(x => (x.mountGuide, x.m1Guide, x.m2Guide))
}

object ArbTelescopeGuideConfig extends ArbTelescopeGuideConfig
