// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.implicits._
import doobie._, doobie.implicits._

import gem.enum.Site
import gem.util.Timestamp
import gsp.math.ProperMotion


object TrackDao {

  /** Obtains, loading from the database if necessary, target tracking data.
    * Enables coordinate queries for a target for the given site and time range.
    */
  def select(t: Target, s: Site, start: Timestamp, end: Timestamp): ConnectionIO[Track] =
    t.track.fold(nonsidereal(_, s, start, end), sidereal)

  private def sidereal(p: ProperMotion): ConnectionIO[Track] =
    Track.Sidereal(p).pure[ConnectionIO].widen[Track]

  private def nonsidereal(k: EphemerisKey, s: Site, start: Timestamp, end: Timestamp): ConnectionIO[Track] =
    for {
      range <- EphemerisDao.bracketRange(k, s, start, end)
      eph   <- EphemerisDao.selectRange(k, s, range._1, range._2)
    } yield Track.Nonsidereal(Map(s -> eph))

}
