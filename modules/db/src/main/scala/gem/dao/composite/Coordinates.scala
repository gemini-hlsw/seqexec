// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import doobie._
import gem.math.{ Angle, HourAngle, Coordinates, Declination, RightAscension }

trait CoordinatesComposite {

  /** Map Coordinates as a (µsec, µasec) pair. */
  implicit val CompositeCoordinates: Composite[Coordinates] =
    Composite[(Long, Long)].imap {
      case (ra, dec) =>
        Coordinates(
          RightAscension(HourAngle.fromMicroseconds(ra)),
          Declination.unsafeFromAngle(Angle.fromMicroarcseconds(dec))
        )
      }(c => (c.ra.toHourAngle.toMicroseconds, c.dec.toAngle.toMicroarcseconds))

}
object CoordinatesComposite extends CoordinatesComposite
