// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.math._

trait RightAscensionMeta {
  import AngleMeta._

  implicit val RightAscensionMeta: Meta[RightAscension] =
    HourAngleMetaAsMicroseconds.xmap(RightAscension(_), _.toHourAngle)

}
object RightAscensionMeta extends RightAscensionMeta
