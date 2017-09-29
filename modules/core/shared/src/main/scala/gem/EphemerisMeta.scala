// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.util.InstantMicros

import cats.{ Eq, Show }


/** Ephemeris meta data related to updates.
  *
  * @param lastUpdate time of last update
  * @param lastUpdateCheck time of last update check
  * @param solnRef horizons solution reference, if any (applies to comet and
  *                asteroid ephemeris data fetched from horizons)
  */
final case class EphemerisMeta(
  lastUpdate: InstantMicros,
  lastUpdateCheck: InstantMicros,
  solnRef: Option[HorizonsSolutionRef])


object EphemerisMeta {

  implicit val eqEphemerisMeta: Eq[EphemerisMeta] =
    Eq.fromUniversalEquals

  implicit val showEphemerisMeta: Show[EphemerisMeta] =
    Show.fromToString
}
