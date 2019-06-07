// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.config.GcalConfig
import gem.config.GcalConfig.{GcalArcs, GcalLamp}
import gem.enum._
import gsp.math.arb.ArbTime

import cats.data.NonEmptySet
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{ Arbitrary, Cogen, Gen }

import java.time.Duration

trait ArbGcalConfig {

  import ArbCoAdds._
  import ArbEnumerated._
  import ArbTime._

  implicit val arbGcalArcs: Arbitrary[GcalArcs] =
    Arbitrary {
      for {
        a  <- arbitrary[GcalArc]
        as <- Gen.someOf(GcalArc.all)
      } yield GcalArcs.of(a, as.toList: _*)
    }

  implicit val cogGcalArcs: Cogen[GcalArcs] =
    Cogen[NonEmptySet[GcalArc]].contramap(_.arcs)

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
        c <- arbitrary[CoAdds      ]
      } yield GcalConfig(l, f, d, s, e, c)
    }

  implicit val cogGcalConfig: Cogen[GcalConfig] =
    Cogen[(GcalLamp, GcalFilter, GcalDiffuser, GcalShutter, Duration, CoAdds)]
      .contramap(g => (g.lamp, g.filter, g.diffuser, g.shutter, g.exposureTime, g.coadds))

}

object ArbGcalConfig extends ArbGcalConfig
