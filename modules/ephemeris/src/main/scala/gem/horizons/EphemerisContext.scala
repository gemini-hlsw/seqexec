// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package horizons

import gem.enum.Site
import gem.util.Timestamp

import cats.implicits._
import scala.math.Ordering.Implicits._


/** Collection of information related to an ephemeris.  Used to determine
  * whether an update is needed.
  *
  * @param key  unique horizons id of the object
  * @param site information valid for the given site
  * @param meta associated ephemeris metadata, if any
  * @param rnge earliest and latest ephemeris element times, if any
  * @param soln current horizons solution reference from JPL, if any
  */
final case class EphemerisContext(
  key:  EphemerisKey.Horizons,
  site: Site,
  meta: Option[EphemerisMeta],
  rnge: Option[(Timestamp, Timestamp)],
  soln: Option[HorizonsSolutionRef]
) {

  /** Determines whether the existing ephemeris, if any, covers the given
    * semester.
    */
  def coversSemester(sem: Semester): Boolean =
    rnge.exists { case (start, end) =>
      (start.toInstant <= sem.start.atSite(site).toInstant) &&
        (sem.end.atSite(site).toInstant <= end.toInstant)
    }

  /** Whether the database and latest horizons version data matches. */
  val sameSolution: Boolean =
    (meta.flatMap(_.solnRef), soln).tupled.exists { case (s0, s1) =>
      s0 === s1
    }

  /** Determines whether updates are needed to obtain the latest ephemeris for
    * the given semester.
    */
  def isUpToDateFor(sem: Semester): Boolean =
    coversSemester(sem) && sameSolution

}
