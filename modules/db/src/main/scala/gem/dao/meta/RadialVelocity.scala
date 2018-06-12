// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.math._

trait RadialVelocityMeta {

  /** Radial velocity in meters per second. */
  implicit lazy val RadialVelocityMeta: Meta[RadialVelocity] =
    Meta[Int].xmap(RadialVelocity.apply, _.toMetersPerSecond)

}
object RadialVelocityMeta extends RadialVelocityMeta
