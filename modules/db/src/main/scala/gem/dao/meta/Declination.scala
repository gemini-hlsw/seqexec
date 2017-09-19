// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.math._

trait DeclinationMeta {
  import AngleMeta._

  implicit val DeclinationMeta: Meta[Declination] =
    AngleMetaAsMicroarcseconds.xmap(Declination.unsafeFromAngle(_), _.toAngle)

}
object DeclinationMeta extends DeclinationMeta
