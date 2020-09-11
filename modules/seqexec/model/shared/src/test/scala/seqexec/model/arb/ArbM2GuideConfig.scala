// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen
import gem.arb.ArbEnumerated.{arbEnumerated => oldArbEnumerated}
import gem.arb.ArbEnumerated.{cogEnumerated => oldCogEnumerated}
import lucuma.core.util.arb.ArbEnumerated._
import seqexec.model.enum.ComaOption
import seqexec.model.enum.TipTiltSource
import seqexec.model.M2GuideConfig

trait ArbM2GuideConfig {

  implicit val arbM2GuideOn: Arbitrary[M2GuideConfig.M2GuideOn] =
    Arbitrary {
      for {
        c <- arbitrary[ComaOption]
        s <- arbitrary[List[TipTiltSource]]
      } yield M2GuideConfig.M2GuideOn(c, s.sortBy(x => s"$x").toSet)
    }

  implicit val m2GuideOnCogen: Cogen[M2GuideConfig.M2GuideOn] =
    Cogen[(ComaOption, List[TipTiltSource])].contramap(x =>
      (x.coma, x.source.toList.sortBy(x => s"$x")))

  implicit val arbM2GuideConfig: Arbitrary[M2GuideConfig] =
    Arbitrary {
      for {
        off <- Gen.const(M2GuideConfig.M2GuideOff)
        on  <- arbitrary[M2GuideConfig.M2GuideOn]
        l   <- Gen.oneOf(off, on)
      } yield l
    }

  implicit val m2GuideConfigCogen: Cogen[M2GuideConfig] =
    Cogen[Option[M2GuideConfig.M2GuideOn]].contramap {
      case x: M2GuideConfig.M2GuideOn => Some(x)
      case _                          => None
    }
}

object ArbM2GuideConfig extends ArbM2GuideConfig
