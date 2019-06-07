// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gsp.math.Epoch

trait EpochMeta {
  import FormatMeta._

  /** Epoch as a string, like J2012.123 */
  implicit lazy val EpochMeta: Meta[Epoch] =
    Epoch.fromString.asMeta

}
object EpochMeta extends EpochMeta
