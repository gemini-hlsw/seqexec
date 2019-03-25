// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Eq, Show }


/** Horizons solution reference.  Comets and asteriods have a unique version
  * that is updated when the ephemeris calculation changes.  For our purposes
  * this is opaque data whose only use is to compare to an earlier version in
  * order to check for changes.
  */
final case class HorizonsSolutionRef(stringValue: String)

object HorizonsSolutionRef {

  implicit val ShowHorizonsSolutionRef: Show[HorizonsSolutionRef] =
    Show.fromToString

  implicit val EqHorizonsSolutionRef: Eq[HorizonsSolutionRef] =
    Eq.fromUniversalEquals
}
