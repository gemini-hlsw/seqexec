// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.math._

trait EpochMeta {

  /** Epoch as a string, like J2012.123 */
  implicit lazy val EpochMeta: Meta[Epoch] =
    Meta[String].xmap(Epoch.unsafeFromString, _.format)

}
object EpochMeta extends EpochMeta
