// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gsp.math.RightAscension

trait RightAscensionMeta {
  import AngleMeta._

  implicit val RightAscensionMeta: Meta[RightAscension] =
    HourAngleMetaAsMicroseconds.timap(RightAscension(_))(_.toHourAngle)

}
object RightAscensionMeta extends RightAscensionMeta
