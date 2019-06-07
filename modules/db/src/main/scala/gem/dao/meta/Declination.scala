// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gsp.math.Declination
import gsp.math.syntax.prism._

trait DeclinationMeta {
  import AngleMeta._

  implicit val DeclinationMeta: Meta[Declination] =
    AngleMetaAsMicroarcseconds.timap(Declination.fromAngle.unsafeGet(_))(_.toAngle)

}
object DeclinationMeta extends DeclinationMeta
