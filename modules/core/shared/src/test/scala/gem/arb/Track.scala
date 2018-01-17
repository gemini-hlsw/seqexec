// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.Site
import gem.math._

import gem.Track.{ Nonsidereal, Sidereal }
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

trait ArbTrack {
  import ArbProperMotion._
  import ArbEnumerated._
  import ArbEphemeris._
  import ArbEphemerisKey._

  implicit val arbSidereal: Arbitrary[Track.Sidereal] =
    Arbitrary {
      arbitrary[ProperMotion].map(Sidereal(_))
    }

  implicit val cogSidereal: Cogen[Track.Sidereal] =
    Cogen[ProperMotion].contramap(_.properMotion)

  implicit val arbNonsidereal: Arbitrary[Track.Nonsidereal] =
    Arbitrary {
      for {
        key  <- arbitrary[EphemerisKey]
        eph  <- arbitrary[Ephemeris]
        site <- arbitrary[Site]
      } yield Nonsidereal(key, Map(site -> eph))
    }

  implicit val cogNonsidereal: Cogen[Track.Nonsidereal] =
    Cogen[(EphemerisKey, Map[Site, Ephemeris])].contramap { n =>
      (n.ephemerisKey, n.ephemerides)
    }

  implicit val arbTrack: Arbitrary[Track] =
    Arbitrary {
      Gen.oneOf(arbitrary[Sidereal], arbitrary[Nonsidereal])
    }

  implicit val cogTrack: Cogen[Track] =
    Cogen[(Option[Sidereal], Option[Nonsidereal])].contramap { t =>
      (t.sidereal, t.nonsidereal)
    }
}

object ArbTrack extends ArbTrack
