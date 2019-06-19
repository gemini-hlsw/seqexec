// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.util.Timestamp

import cats.{ Eq, Show }

import monocle.macros.Lenses


/** Ephemeris meta data related to updates.
  *
  * @param lastUpdate time of last update
  * @param lastUpdateCheck time of last update check
  * @param solnRef horizons solution reference, if any (applies to comet and
  *                asteroid ephemeris data fetched from horizons)
  */
@Lenses final case class EphemerisMeta(
  lastUpdate: Timestamp,
  lastUpdateCheck: Timestamp,
  solnRef: Option[HorizonsSolutionRef]
)

object EphemerisMeta {

  implicit val eqEphemerisMeta: Eq[EphemerisMeta] =
    Eq.fromUniversalEquals

  implicit val showEphemerisMeta: Show[EphemerisMeta] =
    Show.fromToString
}
