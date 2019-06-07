// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.Site
import gem.math.{ LocalObservingNight, ObservingNight }
import gsp.math.arb.ArbTime

import org.scalacheck._
import org.scalacheck.Arbitrary._

import java.time.LocalDate


trait ArbObservingNight {

  import ArbEnumerated._
  import ArbTime._

  implicit val arbLocalObservingNight: Arbitrary[LocalObservingNight] =
    Arbitrary {
      arbitrary[LocalDate].map(LocalObservingNight(_))
    }

  implicit val arbObservingNight: Arbitrary[ObservingNight] =
    Arbitrary {
      for {
        n <- arbitrary[LocalObservingNight]
        s <- arbitrary[Site]
      } yield n.atSite(s)
    }

  implicit val cogLocalObservingNight: Cogen[LocalObservingNight] =
    Cogen[LocalDate].contramap(_.toLocalDate)

  implicit val cogObservingNight: Cogen[ObservingNight] =
    Cogen[(Site, LocalObservingNight)].contramap(o => (o.site, o.toLocalObservingNight))
}

object ArbObservingNight extends ArbObservingNight