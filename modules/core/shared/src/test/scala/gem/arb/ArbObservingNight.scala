// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.Site
import gem.math.ObservingNight

import org.scalacheck._
import org.scalacheck.Arbitrary._

import java.time.Instant


trait ArbObservingNight {

  import ArbEnumerated._
  import ArbTime._

  implicit val arbObservingNight: Arbitrary[ObservingNight] =
    Arbitrary {
      for {
        i <- arbitrary[Instant]
        s <- arbitrary[Site]
      } yield ObservingNight.forInstant(i, s)
    }

  implicit val cogObservingNight: Cogen[ObservingNight] =
    Cogen[(Instant, Instant, Site)].contramap(o => (o.start, o.end, o.site))
}

object ArbObservingNight extends ArbObservingNight