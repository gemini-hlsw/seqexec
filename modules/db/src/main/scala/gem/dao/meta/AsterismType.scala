// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie.Meta
import doobie.postgres.implicits._
import gem.enum.AsterismType

trait AsterismTypeMeta {

  // Workaround until issue #170 is implemented.
  implicit val MetaAsterismType: Meta[AsterismType] =
    pgEnumString("asterism_type", AsterismType.unsafeFromTag, _.tag)

}

object AsterismTypeMeta extends AsterismTypeMeta
