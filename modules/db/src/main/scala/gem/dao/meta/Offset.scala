// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.math.Offset

trait OffsetMeta {
  import AngleMeta._

  // OffsetP maps to a signed angle in arcseconds
  implicit val OffsetPMeta: Meta[Offset.P] =
    AngleMetaAsSignedArcseconds.xmap(Offset.P(_), _.toAngle)

  // OffsetQ maps to a signed angle in arcseconds
  implicit val OffsetQMeta: Meta[Offset.Q] =
    AngleMetaAsSignedArcseconds.xmap(Offset.Q(_), _.toAngle)

}
object OffsetMeta extends OffsetMeta
